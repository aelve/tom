{-# LANGUAGE ViewPatterns, BangPatterns #-}

import Data.Char
import Control.Applicative
import System.FilePath
import System.Directory
import Data.Time
import Data.Time.Zones
import System.Environment
import System.FileLock
import Control.Monad
import Text.Parsec hiding ((<|>))
import Control.DeepSeq
import GHC.Exts (sortWith)
import Data.Foldable (for_)
import System.Random

import Common

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

  * H[.MM][:SS][am/pm][timezone]
    (all in square brackets can be omitted)
    (hour can have 1 or 2 digits)

    examples:
      1.03      – 01.03
      2         – 02.00
      2pm       – 14.00
      2.03:10pm – 14.03:10
--       2pmutc – 14.00 UTC

  * “.MM” – “next occasion it's MM minutes”

  * “:SS” – “next occasion it's SS seconds”

-}
parseTDMask :: String -> String -> IO TDMask
parseTDMask d t = do
  tz <- loadLocalTZ
  ((cYear, cMonth, cDay), (cHour, cMinute, cSecond)) <-
    expandTime tz <$> getCurrentTime
  let counts ns p = choice $ map (try . flip count p) ns
  -- Time parsers.
  let currentTimeP = string "-" *> pure id
      timeMomentP = do
        h <- read <$> counts [2,1] digit
        m <- char '.' *> (read <$> count 2 digit) <|> pure 0
        s <- char ':' *> (read <$> count 2 digit) <|> pure 0
        pm <- choice [ string "pm" *> pure True
                     , string "am" *> pure False
                     , pure False ]
        return $ setHour   (Just (if pm then 12+h else h))
               . setMinute (Just m)
               . setSecond (Just s)
      -- This one should actually be not a time parser, but whole parser. And
      -- it should increment days too (sometimes).
      minuteOccasionP = do
        string "."
        m <- read <$> count 2 digit
        let h = if m <= cMinute then cHour+1 else cHour
        return $ setHour   (Just h)
               . setMinute (Just m)
               . setSecond (Just 0)
      -- See the comment for 'minuteOccasionP'.
      secondOccasionP = do
        string ":"
        s <- read <$> count 2 digit
        let m = if s <= cSecond then cMinute+1 else cMinute
        return $ setMinute (Just m)
               . setSecond (Just s)
  -- Date parsers.
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
          [ (,,) <$> yearP      <*> monthP      <*> dayP
          , (,,) <$> pure cYear <*> monthP      <*> dayP
          , (,,) <$> pure cYear <*> pure cMonth <*> dayP ]
        return $ setYear  (Just y)
               . setMonth (Just m)
               . setDay   (Just d)
      nullDayP = pure id
  -- Combined parsers.
  let timeP = choice $ map try [ currentTimeP, timeMomentP, minuteOccasionP
                               , secondOccasionP ]
      dateP = choice $ map try [dayMomentP, nullDayP]
  let date' = either (error.show) id $ parse (dateP <* eof) "" d
      time' = either (error.show) id $ parse (timeP <* eof) "" t
  let defaultMask = TDMask
        (Just cYear) (Just cMonth) (Just cDay)
        (Just cHour) (Just cMinute) (Just cSecond)
        Nothing Nothing
  return (date' . time' $ defaultMask)

main = do
  args <- getArgs
  if null args
    then listReminders
    else scheduleReminder args

scheduleReminder (dt:msg) = do
  let (ds, ts) = let (a,b) = break (== ',') dt
                 in  if null b then ("", a) else (a, tail b)
  -- Forcing evaluation because otherwise, if something fails, it'll fail
  -- during writing the reminder, and that'd be bad.
  !t <- force <$> parseTDMask ds ts
  newUUID <- randomIO
  time <- getCurrentTime
  let reminder = Reminder
        { mask             = t
        , message          = unwords msg
        , lastSeen         = time
        , lastAcknowledged = time
        , uuid             = newUUID
        }
  withReminderFile $ \f -> do
    appendFile f (show reminder ++ "\n")
    putStrLn "Scheduled a reminder."

listReminders = do
  withReminderFile $ \f -> do
    rs <- readReminders f
    for_ rs $ \r -> do
      putStrLn (show (mask r) ++ ": " ++ message r)
