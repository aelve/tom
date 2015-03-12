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
Months, days and hours can be specified using 1 or 2 digits. Years can be
specified using 2 or 4 digits.

Day format:

  * Y-M-D = year, month, day
            (year can have 2 or 4 digits, month and day – 1 or 2)
  * M-D   = current year, month, day
  * D     = current year, current month, day

Time format:

  * “-” = “current time”

  * H[MM][am/pm][timezone]
    (all in square brackets can be omitted)
    (hour can have 1 or 2 digits)

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
        let dayP   = read <$> counts [2,1] digit
            monthP = read <$> counts [2,1] digit <* string "-"
            yearP  = do
              digits <- counts [4,2] digit
              string "-"
              if length digits == 2
                then return (read ("20" ++ digits))
                else return (read digits)
        (y,m,d) <- choice $ map try
          [ (,,) <$> yearP     <*> monthP     <*> dayP
          , (,,) <$> pure year <*> monthP     <*> dayP
          , (,,) <$> pure year <*> pure month <*> dayP ]
        return (fromGregorian y m d)
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
        (dt:msg) -> let (a,b) = break (== ',') dt
                    in  if null b then ("", a, unwords msg)
                                  else (a, tail b, unwords msg)
  -- Forcing evaluation because otherwise, if something fails, it'll fail
  -- during writing the reminder, and that'd be bad.
  !t <- force <$> guessTime ds ts
  withReminderFile $ \f -> do
    appendFile f (show (Reminder t msg) ++ "\n")
    zonedT <- utcToLocalZonedTime t
    putStrLn ("Scheduled a reminder at " ++ show zonedT ++ ".")
