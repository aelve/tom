{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Tom.Reminders
(
  Acid,
  withDB,
  AddReminder(..),
  EnableReminder(..),
  DisableReminder(..),
  SetMessage(..),
  SetLastSeen(..),
  SetLastAcknowledged(..),
  SetSnoozedUntil(..),
  GetRemindersOn(..),
  GetRemindersOff(..),
  GetReminder(..),
  Reminder(..),
    schedule,
    message,
    created,
    lastSeen,
    lastAcknowledged,
    snoozedUntil,
    secret,
  RemindersFile(..),
    remindersOn,
    remindersOff,
    reminderByUUID,
  isReminderInInterval,
)
where


-- General
import BasePrelude hiding (second)
-- Monads and monad transformers
import Control.Monad.State
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Text
import Data.Text (Text)
-- Files
import System.Directory                   -- directory
import System.FilePath
-- Map
import Data.Map (Map)
-- Time
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock.TAI
import Data.Time.Zones                    -- tz
-- UUIDs
import Data.UUID hiding (null)            -- uuid
-- acid-state & safecopy
import Data.Acid as Acid
import Data.SafeCopy
-- Binary
import Data.Binary (Binary)
import Data.Binary.Orphans ()             -- binary-orphans
-- Tom-specific
import Tom.When
import Tom.Utils


-- | A single reminder.
data Reminder
  = Reminder {
      -- | When the reminder should fire
      _schedule         :: When,
      -- | Message to be shown
      _message          :: Text,
      -- | When the reminder was created
      _created          :: UTCTime,
      -- | When the reminder was last seen by the user (this is used to avoid
      -- showing the same reminder every second once it has fired)
      _lastSeen         :: UTCTime,
      -- | When the reminder was last acknowledged (i.e. the “thanks” button
      -- was pressed) – needed to handle recurring reminders, for which
      -- “acknowledged” doesn't mean “done with”
      _lastAcknowledged :: UTCTime,
      -- | When to start showing the reminder (used for snoozing)
      _snoozedUntil     :: UTCTime,
      -- | Whether reminder text should be hidden by default
      _secret           :: Bool }
  deriving (Eq, Show, Generic, Binary)

data Reminder_v0 = Reminder_v0 {
  _schedule_v0         :: When,
  _message_v0          :: Text,
  _created_v0          :: UTCTime,
  _lastSeen_v0         :: UTCTime,
  _lastAcknowledged_v0 :: UTCTime,
  _snoozedUntil_v0     :: UTCTime }

deriveSafeCopy 0 'base ''Reminder_v0
deriveSafeCopy 1 'extension ''Reminder

instance Migrate Reminder where
  type MigrateFrom Reminder = Reminder_v0
  migrate Reminder_v0{..} = Reminder {
    _schedule         = _schedule_v0,
    _message          = _message_v0,
    _created          = _created_v0,
    _lastSeen         = _lastSeen_v0,
    _lastAcknowledged = _lastAcknowledged_v0,
    _snoozedUntil     = _snoozedUntil_v0,
    _secret           = False }

makeLenses ''Reminder

data RemindersFile = RemindersFile {
  _remindersOn  :: Map UUID Reminder,
  _remindersOff :: Map UUID Reminder }
  deriving (Show)

makeLenses ''RemindersFile

-- | A traversal which gives you access to a reminder by its 'UUID'.
reminderByUUID :: UUID -> Traversal' RemindersFile Reminder
reminderByUUID uuid = failing (remindersOn . ix uuid) (remindersOff . ix uuid)

addReminder :: UUID -> Reminder -> Acid.Update RemindersFile ()
addReminder uuid reminder = do
  remindersOn . at uuid .= Just reminder

enableReminder :: UUID -> Acid.Update RemindersFile ()
enableReminder uuid = do
  file <- get
  case file ^. remindersOff . at uuid of
    Nothing ->
      return ()
    Just reminder -> do
      remindersOn  . at uuid .= Just reminder
      remindersOff . at uuid .= Nothing

disableReminder :: UUID -> Acid.Update RemindersFile ()
disableReminder uuid = do
  file <- get
  case file ^. remindersOn . at uuid of
    Nothing ->
      return ()
    Just reminder -> do
      remindersOff . at uuid .= Just reminder
      remindersOn  . at uuid .= Nothing

setMessage :: UUID -> Text -> Acid.Update RemindersFile ()
setMessage uuid x = do
  reminderByUUID uuid . message .= x

setLastSeen :: UUID -> UTCTime -> Acid.Update RemindersFile ()
setLastSeen uuid x = do
  reminderByUUID uuid . lastSeen .= x

setLastAcknowledged :: UUID -> UTCTime -> Acid.Update RemindersFile ()
setLastAcknowledged uuid x = do
  reminderByUUID uuid . lastAcknowledged .= x

setSnoozedUntil :: UUID -> UTCTime -> Acid.Update RemindersFile ()
setSnoozedUntil uuid x = do
  reminderByUUID uuid . snoozedUntil .= x

getRemindersOn :: Acid.Query RemindersFile (Map UUID Reminder)
getRemindersOn = view remindersOn

getRemindersOff :: Acid.Query RemindersFile (Map UUID Reminder)
getRemindersOff = view remindersOff

getReminder :: UUID -> Acid.Query RemindersFile (Maybe Reminder)
getReminder uuid = preview (reminderByUUID uuid)

deriveSafeCopy 0 'base ''RemindersFile

makeAcidic ''RemindersFile [
  'addReminder,
  'enableReminder, 'disableReminder,
  'setMessage, 'setLastSeen, 'setLastAcknowledged, 'setSnoozedUntil,
  'getRemindersOn, 'getRemindersOff,
  'getReminder ]

nullRemindersFile :: RemindersFile
nullRemindersFile = RemindersFile {
  _remindersOn  = mempty,
  _remindersOff = mempty }

-- | Get the directory where the settings and the reminders file and
-- everything is.
getDataDirectory :: IO FilePath
getDataDirectory = do
  dir <- getAppUserDataDirectory ("aelve" </> "tom")
  createDirectoryIfMissing True dir
  return dir

type Acid = AcidState RemindersFile

withDB :: (Acid -> IO a) -> IO a
withDB act = do
  dir <- getDataDirectory
  bracket (openLocalStateFrom (dir </> "state") nullRemindersFile)
          closeAcidState
          act

-- | Check whether there's -a moment of time which matches the schedule- in
-- a time interval.
--
-- TODO: timeInInterval doesn't work for leap seconds
--
-- TODO: timeInInterval probably fails in presence of DST
timeInInterval
  :: (Int, Int, Int)
  -> (Int, Int, Int)
  -> (Maybe Int, Maybe Int, Maybe Int)
  -> Bool
timeInInterval (ha, ma, sa) (hb, mb, sb) (hx, mx, sx) =
  -- It's not necessary to add e.g. 59 or 0 to the lists for “m” or “s”.
  -- I checked by bruteforcing.
  not . null $ [(h,m,s) | h <- maybe [ha, min 23 (ha+1), hb] pure hx
                        , m <- maybe [0, ma, min 59 (ma+1), mb] pure mx
                        , s <- maybe [sa, sb] pure sx
                        , (ha,ma,sa) <= (h,m,s)
                        , (hb,mb,sb) >= (h,m,s) ]      

{- |
Check whether a reminder has fired in a time interval. Not as efficient as it could be (it takes O(days in the interval) for 'Mask'). 'IO' is needed to look up the timezone from the name possibly contained in the schedule.
-}
isReminderInInterval :: (UTCTime, UTCTime) -> When -> IO Bool
isReminderInInterval (a, b) Moment{..} =
  return (utcToAbsoluteTime a <= moment && moment <= utcToAbsoluteTime b)
isReminderInInterval (a, b) Periodic{..} = do
  -- x      x      x      x      x      ...
  --      |    |
  --      a    b
  -- 
  -- Each “x” denotes a single firing of the periodic reminder; the distance
  -- between “x”s is ‘period’, the 1st “x” happens at moment ‘start’. To
  -- check whether any of “x”s is in the a..b interval, we first have to
  -- shift everything in time (so that the 1st “x” would be 0) and then check
  -- whether ‘a `div` period’ and ‘b `div` period’ are different.
  let a' = diffAbsoluteTime (utcToAbsoluteTime a) start
      b' = diffAbsoluteTime (utcToAbsoluteTime b) start
  return $
    utcToAbsoluteTime b >= start &&
    a' `div'` period /= (b' `div'` period :: Integer)
isReminderInInterval (a, b) Mask{..} = do
  -- If timezone is specified in the reminder, use it, and otherwise use the
  -- local one.
  tz <- case timezone of
    Nothing -> loadLocalTZ
    Just x  -> loadSystemTZ x

  -- Break endpoints of the interval into parts (using the timezone we know).
  let (dateA@(yearA,monthA,dayA),timeA) = expandTime tz (ceilingUTCTime a)
      (dateB@(yearB,monthB,dayB),timeB) = expandTime tz (floorUTCTime b)

  -- Use the mask to decide on which days the time specified in the mask can
  -- occur (after that we can stop being bothered about time at all). When a
  -- day is whole, we assume that it contains *all* possible times (it's a
  -- lie, because when DST kicks in, a day loses or gains an additional hour
  -- – but we ignore that), and therefore we only have to check the first and
  -- last days, because they aren't whole.

  -- endA   = “when does the first day end”
  -- startB = “when does the last day start”
  --
  -- TODO: it should be (23,59,60) when a leap second is involved
  let (endA, startB) | dateA == dateB = (timeB, timeA)
                     | otherwise      = ((23,59,59),(0,0,0))

  -- First and last days as “Day”s instead of tuples of year-month-day – this
  -- is needed to be able to enumerate them easily later using “..”.
  let julianA, julianB :: Day
      julianA = fromGregorian yearA monthA dayA
      julianB = fromGregorian yearB monthB dayB

  -- dateA' = “the first day when given time actually occurs”
  -- dateB' = “the last day when given time actually occurs”
  let timeMask = (hour, minute, second)
      dateA' | timeInInterval timeA endA timeMask   = julianA
             | otherwise                            = succ julianA
      dateB' | timeInInterval startB timeB timeMask = julianB
             | otherwise                            = pred julianB

  -- A function to check whether a day fits the given mask.
  let check whole@(toGregorian -> (y,m,d)) = and [
        maybe True (== y) year,
        maybe True (== m) month,
        maybe True (== d) day,
        maybe True (snd (mondayStartWeek whole) `elem`) weekdays ]

  -- Now we have all the pieces, and can just enumerate all possible days,
  -- check each for fitting the mask, and say whether we found a day we want.
  return (not . null . filter check $ [dateA' .. dateB'])
