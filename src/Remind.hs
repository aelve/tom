{-# LANGUAGE ViewPatterns, BangPatterns #-}

import Data.Char
import Control.Applicative
import System.FilePath
import System.Directory
import Data.Time
import System.Environment
import System.FileLock
import Control.Monad
import Text.Parsec hiding ((<|>))
import Control.DeepSeq

data Reminder = Reminder {
    time    :: UTCTime
  , message :: String
  }
  deriving (Read, Show)

getDir = do
  dir <- getAppUserDataDirectory "remind"
  ex <- doesDirectoryExist dir
  unless ex $
    createDirectory dir
  return dir

withReminderFile action = do
  dir <- getDir
  withFileLock (dir </> "lock") Exclusive $ \_ -> do
    let fileName = (dir </> "reminders")
    ex <- doesFileExist fileName
    unless ex $
      writeFile fileName ""
    action fileName

{-
Day format:

  * (M|MM)-(D|DD) = month, day
  * D|DD = current month, day

Time format:

  * “-” = “current time”

  * (H|HH)[MM][am/pm][timezone] (all in square brackets can be omitted)

    examples:
      103    – 01.03
      2      – 02.00
      2pm    – 14.00
--       2pmutc – 14.00 UTC

  * “.MM” – “next occasion it's MM minutes”

-}
guessTime :: String -> String -> IO UTCTime
guessTime d t = do
  ZonedTime (LocalTime (toGregorian->(year,month,day)) time) zone <-
    getZonedTime
  let counts ns p = choice $ map (try . flip count p) ns
  -- TODO: make parsers return time-changing functions, it could work nicer
  -- Time parsers.
  let currentTimeP = string "-" *> pure time
      timeMomentP = do
        (h, m) <- choice $ map try
          [ do h <- read <$> count 2 digit
               m <- read <$> count 2 digit
               return (h, m)
          , do h <- read <$> count 1 digit
               m <- read <$> count 2 digit
               return (h, m)
          , do h <- read <$> counts [2,1] digit
               return (h, 0)
          ]
        pm <- choice [ string "pm" *> pure True
                     , string "am" *> pure False
                     , pure False ]
        return (TimeOfDay (if pm then 12+h else h) m 0)
      minuteOccasionP = do
        string "."
        m <- read <$> count 2 digit
        let h = (if m <= todMin time then succ else id) (todHour time)
        return (TimeOfDay h m 0)
  -- Day parsers.
  let dayMomentP = do
        m <- choice $ map try
          [ read <$> counts [2,1] digit <* string "-"
          , pure month
          ]
        d <- read <$> counts [2,1] digit
        return (fromGregorian year m d)
      nullDayP = pure (fromGregorian year month day)
  -- Combined parsers.
  let timeP = choice $ map try [currentTimeP, timeMomentP, minuteOccasionP]
      dayP  = choice $ map try [dayMomentP, nullDayP]
  let day'  = either (error.show) id $ parse (dayP <* eof) "" d
  let time' = either (error.show) id $ parse (timeP <* eof) "" t
  return (zonedTimeToUTC (ZonedTime (LocalTime day' time') zone))

main = do
  args <- getArgs
  let (ds, ts, msg) = case args of
        [msg]         -> ("", "5m", msg)
        [ts, msg]     -> ("", ts  , msg)
        [ds, ts, msg] -> (ds, ts  , msg)
  -- Forcing evaluation because otherwise, if something fails, it'll fail
  -- during writing the reminder, and that'd be bad.
  !t <- force <$> guessTime ds ts
  withReminderFile $ \f -> do
    appendFile f (show (Reminder t msg) ++ "\n")
    zonedT <- utcToLocalZonedTime t
    putStrLn ("Scheduled a reminder at " ++ show zonedT ++ ".")
