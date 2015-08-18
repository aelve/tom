{-# LANGUAGE
RecordWildCards,
ViewPatterns,
DeriveGeneric,
OverloadedStrings,
TemplateHaskell,
RankNTypes
  #-}


module Tom.Common
(
  Reminder(..),
    schedule,
    message,
    created,
    lastSeen,
    lastAcknowledged,
    snoozedUntil,
  RemindersFile(..),
    remindersOn,
    remindersOff,
    reminder,
  readRemindersFile,
  withRemindersFile,
  enableReminder,
  disableReminder,
  addReminder,
  modifyReminder,
  isReminderInInterval,
)
where


-- General
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.Monoid
-- Lenses
import           Lens.Micro.Platform hiding ((.=))
-- Files
import           System.Directory                   -- directory
import           System.FilePath                    -- filepath
-- File locking
import           System.FileLock                    -- filelock
-- ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
-- Map
import qualified Data.Map as M
import           Data.Map (Map)
-- Time
import           Data.Time
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Zones                    -- tz
-- UUIDs
import           Data.UUID hiding (null)            -- uuid
-- JSON
import           Data.Aeson as Aeson                -- aeson
import qualified Data.Aeson.Encode.Pretty as Aeson  -- aeson-pretty
-- Randomness
import           System.Random
-- IORef (used for caching the reminders file)
import           Data.IORef
-- unsafePerformIO
import           System.IO.Unsafe (unsafePerformIO)
-- Tom-specific
import           Tom.Time
import           Tom.When


-- | A single reminder.
data Reminder
  = Reminder {
      -- | When the reminder should fire
      _schedule         :: When,
      -- | Message to be shown
      _message          :: String,
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
      _snoozedUntil     :: UTCTime }
  deriving (Eq, Read, Show)

makeLenses ''Reminder

instance FromJSON Reminder where
  parseJSON = withObject "reminder" $ \o -> do
    _schedule         <- read <$> o .: "schedule"
    _message          <- o .: "message"
    _created          <- o .: "created"
    _lastSeen         <- o .: "seen"
    _lastAcknowledged <- o .: "acknowledged"
    _snoozedUntil     <- o .: "snoozed-until"
    return Reminder{..}

instance ToJSON Reminder where
  toJSON Reminder{..} = object [
    "schedule"      .= show _schedule,
    "message"       .= _message,
    "created"       .= _created,
    "seen"          .= _lastSeen,
    "acknowledged"  .= _lastAcknowledged,
    "snoozed-until" .= _snoozedUntil ]

data RemindersFile = RemindersFile {
  _remindersOn  :: Map UUID Reminder,
  _remindersOff :: Map UUID Reminder }
  deriving (Read, Show)

makeLenses ''RemindersFile

-- | A traversal which gives you access to a reminder by its 'UUID'.
reminder :: UUID -> Traversal' RemindersFile Reminder
reminder uuid = failing (remindersOn . ix uuid) (remindersOff . ix uuid)

nullRemindersFile :: RemindersFile
nullRemindersFile = RemindersFile {
  _remindersOn  = mempty,
  _remindersOff = mempty }

instance FromJSON RemindersFile where
  parseJSON = withObject "reminders file" $ \o -> do
    _remindersOn  <- M.mapKeys read <$> o .: "on"
    _remindersOff <- M.mapKeys read <$> o .: "off"
    return RemindersFile{..}

instance ToJSON RemindersFile where
  toJSON RemindersFile{..} = object [
    "on"  .= M.mapKeys show _remindersOn,
    "off" .= M.mapKeys show _remindersOff ]

-- | Get the directory where the settings and the reminders file and
-- everything is.
getDataDirectory :: IO FilePath
getDataDirectory = do
  dir <- getAppUserDataDirectory "aelve/tom"
  ex <- doesDirectoryExist dir
  unless ex $
    createDirectory dir
  return dir

-- | Just read the reminders file, without setting up any locks and so on.
--
-- This function, however, does implement caching – if the reminders file
-- hasn't changed, it will be simply taken from the cache instead of being
-- reread and reparsed.
--
-- This function doesn't assume that the file exists, and will recreate it if
-- it doesn't.
readRemindersFileWithoutLocking :: IO RemindersFile
readRemindersFileWithoutLocking = do
  dir <- getDataDirectory
  let filename = dir </> "reminders"
  exists <- doesFileExist filename
  when (not exists) $
    BSL.writeFile filename (Aeson.encodePretty nullRemindersFile)
  fileChanged <- getModificationTime filename
  cache <- readIORef cacheRef
  case cache of
    Just (cacheCreated, file) | cacheCreated > fileChanged ->
      return file
    _other -> do
      contents <- BSL.fromStrict <$> BS.readFile filename
      let err  = error "Can't parse reminders."
          file = fromMaybe err (Aeson.decode' contents)
      writeIORef cacheRef (Just (fileChanged, file))
      return file
  where
    cacheRef :: IORef (Maybe (UTCTime, RemindersFile))
    cacheRef = unsafePerformIO $ newIORef Nothing

readRemindersFile :: IO RemindersFile
readRemindersFile = do
  dir <- getDataDirectory
  withFileLock (dir </> "lock") Exclusive $ \_ -> do
    readRemindersFileWithoutLocking

enableReminder :: UUID -> IO ()
enableReminder uuid = withRemindersFile $ \file ->
  return $ case file ^. remindersOff . at uuid of
    Nothing -> file
    Just reminder -> file & remindersOn  . at uuid .~ Just reminder
                          & remindersOff . at uuid .~ Nothing

disableReminder :: UUID -> IO ()
disableReminder uuid = withRemindersFile $ \file ->
  return $ case file ^. remindersOn . at uuid of
    Nothing -> file
    Just reminder -> file & remindersOn  . at uuid .~ Nothing
                          & remindersOff . at uuid .~ Just reminder

modifyReminder :: UUID -> (Reminder -> Reminder) -> IO ()
modifyReminder uuid f = withRemindersFile $ \file ->
  return $ file & reminder uuid %~ f

-- | Add a (turned on) reminder. The UUID will be generated automatically.
addReminder :: Reminder -> IO ()
addReminder reminder = withRemindersFile $ \file -> do
  uuid <- randomIO
  return $ file & remindersOn . at uuid .~ Just reminder

withRemindersFile :: (RemindersFile -> IO RemindersFile) -> IO ()
withRemindersFile func = do
  dir <- getDataDirectory
  withFileLock (dir </> "lock") Exclusive $ \_ -> do
    file <- readRemindersFileWithoutLocking
    -- Just writing encoded data to the file isn't safe, because if something
    -- happens while we're writing (such as power outage), we risk losing all
    -- reminders. So, instead we're going to write into a *different* file,
    -- and then atomically (or so documentation for 'renameFile' claims)
    -- rename the new one into the old one. Note: we can't create this file
    -- in a temporary directory, because it might not be on the same device,
    -- which would cause renameFile to fail.
    let newFile = dir </> "reminders-new"
    BSL.writeFile newFile . Aeson.encodePretty =<< func file
    renameFile newFile (dir </> "reminders")

-- | Check whether there's -a moment of time which matches the schedule- in
-- a time interval.
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

-- | Round 'UTCTime' down to have a whole number of seconds.
floorUTCTime :: UTCTime -> UTCTime
floorUTCTime t = t {utctDayTime = fromInteger (floor (utctDayTime t))}

-- | Round 'UTCTime' up to have a whole number of seconds.
--
-- It ignores leap seconds (the result here was expected to be “23:59:60”):
--
-- >>> ceilingUTCTime (read "2012-06-30 23:59:59.99")
-- 2012-07-01 00:00:00 UTC
ceilingUTCTime :: UTCTime -> UTCTime
ceilingUTCTime t
  | d >= 86400 = t { utctDay     = addDays 1 (utctDay t),
                     utctDayTime = 0 }
  | otherwise  = t { utctDayTime = fromInteger d }
  where
    d = ceiling (utctDayTime t)

-- | Check whether a reminder has fired in a time interval. Not as efficient
-- as it could be (it takes O(days in the interval)). 'IO' is needed to look
-- up the timezone from the name contained in the schedule.
isReminderInInterval :: (UTCTime, UTCTime) -> When -> IO Bool
isReminderInInterval (ceilingUTCTime -> a, floorUTCTime -> b) Mask{..} = do
  -- If timezone is specified in the reminder, use it, and otherwise use the
  -- local one.
  tz <- case timezone of
    Nothing -> loadLocalTZ
    Just x  -> loadSystemTZ x

  -- Break endpoints of the interval into parts (using the timezone we know).
  let (dateA@(yearA,monthA,dayA),timeA) = expandTime tz a
      (dateB@(yearB,monthB,dayB),timeB) = expandTime tz b

  -- Use the mask to decide on which days the time specified in the mask can
  -- occur (after that we can stop being bothered about time at all). When a
  -- day is whole, we assume that it contains *all* possible times (it's a
  -- lie, because when DST kicks in, a day loses or gains an additional hour
  -- – but we ignore that), and therefore we only have to check the first and
  -- last days, because they aren't whole.

  -- endA   = “when does the first day end”
  -- startB = “when does the last day start”
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
