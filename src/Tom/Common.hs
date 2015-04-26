{-# LANGUAGE
RecordWildCards,
ViewPatterns,
DeriveGeneric,
OverloadedStrings
  #-}


module Tom.Common
(
  Reminder(..),
  RemindersFile(..),
  readRemindersFile,
  withRemindersFile,
  enableReminder,
  disableReminder,
  addReminder,
  modifyReminder,
  reminderInInterval,
)
where


-- General
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.Monoid
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
-- Tom-specific
import           Tom.Time
import           Tom.When


-- | A single reminder.
data Reminder
  = Reminder {
      -- | When the reminder should fire
      schedule         :: When,
      -- | Message to be shown
      message          :: String,
      -- | When the reminder was created
      created          :: UTCTime,
      -- | When the reminder was last seen by the user (this is used to avoid
      -- showing the same reminder every second once it has fired)
      lastSeen         :: UTCTime,
      -- | When the reminder was last acknowledged (i.e. the “thanks” button
      -- was pressed) – needed to handle recurring reminders, for which
      -- “acknowledged” doesn't mean “done with”
      lastAcknowledged :: UTCTime,
      -- | When to start showing the reminder (used for snoozing)
      ignoreUntil      :: UTCTime }
  deriving (Eq, Read, Show)

instance FromJSON Reminder where
  parseJSON = withObject "reminder" $ \o -> do
    schedule         <- read <$> o .: "schedule"
    message          <- o .: "message"
    created          <- o .: "created"
    lastSeen         <- o .: "seen"
    lastAcknowledged <- o .: "acknowledged"
    ignoreUntil      <- o .: "ignore-until"
    return Reminder{..}

instance ToJSON Reminder where
  toJSON Reminder{..} = object [
    "schedule"     .= show schedule,
    "message"      .= message,
    "created"      .= created,
    "seen"         .= lastSeen,
    "acknowledged" .= lastAcknowledged,
    "ignore-until" .= ignoreUntil ]

data RemindersFile
  = RemindersFile {
      remindersOn  :: Map UUID Reminder,
      remindersOff :: Map UUID Reminder }
  deriving (Read, Show)

nullRemindersFile :: RemindersFile
nullRemindersFile = RemindersFile {
  remindersOn  = mempty,
  remindersOff = mempty }

instance FromJSON RemindersFile where
  parseJSON = withObject "reminders file" $ \o -> do
    remindersOn  <- M.mapKeys read <$> o .: "on"
    remindersOff <- M.mapKeys read <$> o .: "off"
    return RemindersFile{..}

instance ToJSON RemindersFile where
  toJSON RemindersFile{..} = object [
    "on"  .= M.mapKeys show remindersOn,
    "off" .= M.mapKeys show remindersOff ]

getDir = do
  dir <- getAppUserDataDirectory "aelve/tom"
  ex <- doesDirectoryExist dir
  unless ex $
    createDirectory dir
  return dir

readRemindersFile :: IO RemindersFile
readRemindersFile = do
  dir <- getDir
  withFileLock (dir </> "lock") Exclusive $ \_ -> do
    let fileName = dir </> "reminders"
    exists <- doesFileExist fileName
    when (not exists) $
      BSL.writeFile fileName (Aeson.encodePretty nullRemindersFile)
    contents <- BSL.fromStrict <$> BS.readFile fileName
    let err = error "Can't parse reminders."
    return $ fromMaybe err (Aeson.decode' contents)

withRemindersFile :: (RemindersFile -> IO RemindersFile) -> IO ()
withRemindersFile func = do
  dir <- getDir
  withFileLock (dir </> "lock") Exclusive $ \_ -> do
    let remFile = dir </> "reminders"
    exists <- doesFileExist remFile
    when (not exists) $
      BSL.writeFile remFile (Aeson.encodePretty nullRemindersFile)
    contents <- BSL.fromStrict <$> BS.readFile remFile
    let err  = error "Can't parse reminders."
        file = fromMaybe err (Aeson.decode' contents)
    -- Just writing encoded data to the file isn't safe, because if something
    -- happens while we're writing (such as power outage), we risk losing all
    -- reminders. So, instead we're going to write into a *different* file,
    -- and then atomically (or so documentation for 'renameFile' claims)
    -- rename the new one into the old one. Note: we can't create this file
    -- in a temporary directory, because it might not be on the same device,
    -- which would cause renameFile to fail.
    let newFile = dir </> "reminders-new"
    BSL.writeFile newFile . Aeson.encodePretty =<< func file
    renameFile newFile remFile

enableReminder :: UUID -> (RemindersFile -> RemindersFile)
enableReminder u file = fromMaybe file $ do
  r <- M.lookup u (remindersOff file)
  return file { remindersOn  = M.insert u r (remindersOn file),
                remindersOff = M.delete u (remindersOff file) }

disableReminder :: UUID -> (RemindersFile -> RemindersFile)
disableReminder u file = fromMaybe file $ do
  r <- M.lookup u (remindersOn file)
  return file { remindersOn  = M.delete u (remindersOn file),
                remindersOff = M.insert u r (remindersOff file) }

modifyReminder :: UUID -> (Reminder -> Reminder)
                       -> (RemindersFile -> RemindersFile)
modifyReminder u f file
  | M.member u (remindersOn file) =
      file { remindersOn = M.adjust f u (remindersOn file) }
  | otherwise =
      file { remindersOff = M.adjust f u (remindersOff file) }

-- | Add a (turned on) reminder. The UUID will be generated automatically.
addReminder :: Reminder -> (RemindersFile -> IO RemindersFile)
addReminder r file = do
  u <- randomIO
  return file { remindersOn = M.insert u r (remindersOn file) }

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
reminderInInterval
  :: UTCTime       -- ^ Beginning of the interval.
  -> UTCTime       -- ^ End of the interval.
  -> When          -- ^ Reminder's schedule.
  -> IO Bool
reminderInInterval (ceilingUTCTime -> a) (floorUTCTime -> b) Mask{..} = do
  -- If timezone is specified in the reminder, use it, and otherwise use the
  -- local one.
  tz <- fromMaybe loadLocalTZ (loadSystemTZ <$> timezone)
  let (dateA@(yearA,monthA,dayA),timeA) = expandTime tz a
      (dateB@(yearB,monthB,dayB),timeB) = expandTime tz b
  -- Use the time mask to decide which days are acceptable (after that we can
  -- stop being bothered about time at all). Basically, we only have to check
  -- whether the 1st and last days match, and the rest always do.
  let (endA, startB) | dateA == dateB = (timeB, timeA)
                     | otherwise      = ((23,59,59),(0,0,0))
      julianA = fromGregorian yearA monthA dayA
      julianB = fromGregorian yearB monthB dayB
      timeMask = (hour, minute, second)
      dateA' | timeInInterval timeA endA timeMask   = julianA
             | otherwise                            = succ julianA
      dateB' | timeInInterval startB timeB timeMask = julianB
             | otherwise                            = pred julianB
  let check whole@(toGregorian -> (y,m,d)) = and [
        maybe True (== y) year,
        maybe True (== m) month,
        maybe True (== d) day,
        maybe True (snd (mondayStartWeek whole) `elem`) weekdays ]
  return (not . null . filter check $ [dateA' .. dateB'])
