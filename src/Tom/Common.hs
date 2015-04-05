{-# LANGUAGE
  RecordWildCards
, ViewPatterns
, DeriveGeneric
  #-}

module Tom.Common
(
  Reminder(..),
  getDir,
  withReminderFile,
  readReminders,
  modifyReminder,
  expandTime,
  reminderInInterval,
  tzNameToOlson,
  olsonToTZName,
)
where

import Control.Applicative
import Control.Monad
import System.Directory
import System.FilePath
import System.FileLock
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Zones
import qualified System.IO.Strict as Strict
import Data.Maybe
import Data.UUID hiding (null)

import Tom.Time
import Tom.Mask

data Reminder = Reminder
  { mask             :: Mask
  , message          :: String
  , lastSeen         :: UTCTime
  , lastAcknowledged :: UTCTime
  , uuid             :: UUID
  }
  deriving (Eq, Read, Show)

getDir = do
  dir <- getAppUserDataDirectory "aelve/tom"
  ex <- doesDirectoryExist dir
  unless ex $
    createDirectory dir
  return dir

withReminderFile action = do
  dir <- getDir
  withFileLock (dir </> "lock") Exclusive $ \_ -> do
    let fileName = dir </> "reminders"
    ex <- doesFileExist fileName
    unless ex $
      writeFile fileName ""
    action fileName

-- | Should be called in 'withReminderFile'.
readReminders :: FilePath -> IO [Reminder]
readReminders f = map read . lines <$> Strict.readFile f

modifyReminder :: FilePath -> UUID -> (Reminder -> Reminder) -> IO ()
modifyReminder f u func = do
  rs <- readReminders f
  let rs' = map (\r -> if uuid r == u then func r else r) rs
  writeFile f (unlines (map show rs'))

-- | Check whether there's -a moment of time which matches the mask- in a
-- time interval.
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
  | d >= 86400 = t { utctDay     = addDays 1 (utctDay t)
                   , utctDayTime = 0 }
  | otherwise  = t { utctDayTime = fromInteger d }
  where
    d = ceiling (utctDayTime t)

-- | Check whether a reminder has fired in a time interval. Not as efficient
-- as it could be (it takes O(days in the interval)). 'IO' is needed to look
-- up the timezone from the name contained in the mask.
reminderInInterval
  :: UTCTime       -- ^ Beginning of the interval.
  -> UTCTime       -- ^ End of the interval.
  -> Mask          -- ^ Reminder's time mask.
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
  let check whole@(toGregorian -> (y,m,d)) = and
        [ maybe True (== y) year
        , maybe True (== m) month
        , maybe True (== d) day
        , maybe True (snd (mondayStartWeek whole) `elem`) weekdays
        ]
  return (not . null . filter check $ [dateA' .. dateB'])
