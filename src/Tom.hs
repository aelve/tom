{-# LANGUAGE
      ViewPatterns
    , BangPatterns
    , TupleSections
  #-}

import Data.Char
import Control.Applicative
import System.FilePath
import System.Directory
import Data.Time
import Data.Time.Calendar.MonthDay
import Data.Time.Zones
import System.Environment
import System.FileLock
import Control.Monad
import Text.Parsec hiding ((<|>), optional)
import Text.Parsec.String
import Control.DeepSeq
import GHC.Exts (sortWith)
import Data.Foldable (asum, for_)
import System.Random
import Data.Maybe
import Data.List (find)
import Control.Exception (evaluate)

import Common

-- | A parser for masks ('TDMask'), with a twist – the mask can depend on
-- current time.
type TDParser = Parser (TZ -> UTCTime -> TDMask)

-- | A parser for nonnegative integers (0, 1, 2, ...).
nonnegative :: (Read a, Integral a) => Parser a
nonnegative = read <$> many1 digit

{- |
A parser for moments in time.

All numbers can be specified using any amount of digits. Date and time are
separated with a comma. Date can be omitted.

Date format:

  * Y-M-D
  * M-D
  * D

Time format: [H][.MM][:SS][am/pm] (all components can be omitted)

The next suitable time is always chosen. For instance, if it's past 9pm
already, then “9pm” will resolve to 9pm of the next day. In the same way,
“2-29,8am” refers to morning of the next February 29 (which may happen in as
much as 4 years).

If the resulting time is always in the past, the function will fail.
-}
momentP :: TDParser
momentP = do
  -- This helper function reads year from a string, accounting for the
  -- “assume current millenium” shortcut.
  --
  --   - "10"   -> 2010
  --   - "112"  -> 2112
  --   - "2020" -> 2020
  --   - "3388" -> 3388
  let readYear s = if length s < 4 then 2000 + read s else read s

  -- These are parsers for parts of date. Year and month are always followed
  -- with “-”, and day is always followed with “,” (the date/time separator).
  let yearP  :: Parser Integer
      monthP :: Parser Int
      dayP   :: Parser Int

      yearP   = (readYear <$> many1 digit) <* string "-"
      monthP  = nonnegative <* string "-"
      dayP    = nonnegative <* string ","

  -- Parsing date: it's either “Y-M-D”, “M-D”, “D”, or nothing (and then
  -- there is no comma). We use Just to denote that year/month/day is set.
  (mbYear, mbMonth, mbDay) <- choice $ map try
    [ do y <- yearP; m <- monthP; d <- dayP; return (Just y, Just m, Just d)
    , do             m <- monthP; d <- dayP; return (Nothing,Just m, Just d)
    , do                          d <- dayP; return (Nothing,Nothing,Just d)
    , return (Nothing, Nothing, Nothing) ]

  -- Parsing time is more complicated. We parse
  -- [hour][.minute][:second][am|pm], where every component is
  -- optional. Luckily, it's easy to distinguish between them.
  (mbHour, mbMinute, mbSecond) <- do
    h <- optional nonnegative
    -- If hour is set and minute isn't, it's assumed to be 0, *not*
    -- omitted. For instance, “10am” really means “10.00:00”.
    m <- Just <$> (char '.' *> nonnegative)           <|>
         pure (if isJust h then Just 0 else Nothing)
    -- Same for minute/second.
    s <- Just <$> (char ':' *> nonnegative)           <|>
         pure (if isJust m then Just 0 else Nothing)
    -- 1. “pm” means “add 12 to hour”.
    -- 2. Unless it's 12pm, in which case it doesn't.
    -- 3. And “am” can't be ignored, because 12am ≠ 12.00.
    -- 4. 13am shall mean 1.00. For consistency.
    pm <- choice [ string "pm" *> pure True
                 , string "am" *> pure False
                 , pure False ]
    let fromPM x = if x >= 12 then x else x + 12
        fromAM x = if x <  12 then x else x - 12
    -- And now we can return parsed hour, minute, and second.
    return ((if pm then fromPM else fromAM) <$> h, m, s)

  -- Finally, we have to fill in the blanks (“Nothing”) so that the result is
  -- the *least* possible time which is still bigger than the current time.
  return $ \tz time -> do

    -- The parser returns a function which is given current time and
    -- timezone. We're inside this function now, and we turn given time and
    -- timezone into something more usable – namely, year/month/day/etc.
    let ((cYear,cMonth,cDay),(cHour,cMinute,cSecond)) = expandTime tz time

    -- These are lowest possible second, minute+second, hour+minute+second,
    -- etc. (where “possible” means “don't contradict filled parts of the
    -- time mask”). For convenience, they are built as nested tuples.
    let zeroSecond =   fromMaybe 0 mbSecond
        zeroMinute = ( fromMaybe 0 mbMinute , zeroSecond )
        zeroHour   = ( fromMaybe 0 mbHour   , zeroMinute )
        zeroDay    = ( fromMaybe 1 mbDay    , zeroHour   )
        zeroMonth  = ( fromMaybe 1 mbMonth  , zeroDay    )

    -- The following functions compute the lowest possible second,
    -- minute+second, hour+minute+second, etc. which is *bigger than
    -- given*. They return Nothing if the given time can't be incremented.
    let nextSecond x = case mbSecond of
          Just y  -> if x < y then Just y else Nothing
          Nothing -> if x <= 58 then Just (x+1) else Nothing

    let nextMinute (x, rest) = case mbMinute of
          -- If we *have* to use mbMinute, we only decide whether we have to
          -- increment the second or not.
          Just y
            | x < y  -> Just (y, zeroSecond)
            | x == y -> (y,) <$> nextSecond rest
            | x > y  -> Nothing
          -- If we don't have to use mbMinute, we try to increment the second
          -- first, and increase the minute if we can't increment the second.
          Nothing    -> (x,) <$> nextSecond rest  <|>
                        (guard (x <= 58) >> return (x+1, zeroSecond))

    -- nextHour is the same as nextMinute.
    let nextHour (x, rest) = case mbHour of
          Just y
            | x < y  -> Just (y, zeroMinute)
            | x == y -> (y,) <$> nextMinute rest
            | x > y  -> Nothing
          Nothing    -> (x,) <$> nextMinute rest  <|>
                        (guard (x <= 22) >> return (x+1, zeroMinute))

    -- nextDay is special – whether you can increment the day or not depends
    -- on the month. Therefore, it has to be passed the number of days in the
    -- month. Moreover, it's possible that the day from the mask ('mbDay')
    -- can't fit into month – in this case we return Nothing.
    let nextDay days (x, rest) = do
          -- It's easier to compute first and check whether the day fits
          -- afterwards.
          (x', rest') <- case mbDay of
            Just y
              | x < y  -> Just (y, zeroHour)
              | x == y -> (y,) <$> nextHour rest
              | x > y  -> Nothing
            Nothing    -> (x,) <$> nextHour rest  <|>
                          Just (x+1, zeroHour)
          guard (x' <= days)
          return (x', rest')

    -- nextMonth is special too, because the amount of days in the month
    -- depends on the year. So, we have to know whether the year is
    -- leap. Moreover, additional trouble comes with the fact that it may not
    -- be enough to increment once – we try incrementing both once and twice
    -- to make sure we touch at least one month with 31 days in it.
    let nextMonth isLeap (x, rest) = case mbMonth of
          Just y
            | x < y  -> Just (y, zeroDay)
            | x == y -> (y,) <$> nextDay (monthLength isLeap y) rest
            | x > y  -> Nothing
          Nothing    -> asum
            -- Increment day.
            [ (x,) <$> nextDay (monthLength isLeap x) rest
            -- Increment month once, and check whether the day fits.
            , do guard (x <= 11)
                 guard (fst zeroDay <= monthLength isLeap (x+1))
                 return (x+1, zeroDay)
            -- Increment month twice, and check.
            , do guard (x <= 10)
                 guard (fst zeroDay <= monthLength isLeap (x+2))
                 return (x+2, zeroDay)
            ]

    -- Phew, almost done.
    let nextYear (x, rest) = case mbYear of
          Just y
            | x < y  -> Just (y, zeroMonth)
            | x == y -> (y,) <$> nextMonth (isLeapYear y) rest
            | x > y  -> Nothing
          Nothing    -> asum
            -- Increment month.
            [ (x,) <$> nextMonth (isLeapYear x) rest
            -- Increment year once, see if day and month fit. (They can only
            -- not fit if the year isn't leap but it's February 29.)
            , do let (m,(d,_)) = zeroMonth
                 guard $ not (m == 2 && d == 29 && not (isLeapYear (x+1)))
                 return (x+1, zeroMonth)
            -- Increment year until it's a leap one.
            , return (fromJust (find isLeapYear [x+2..]), zeroMonth)
            ]

    -- Now we simply use nextYear to increment the current time, which would
    -- give us next time which fits the mask.
    let (year', (month', (day', (hour', (minute', second'))))) =
          fromMaybe 
            (error "Can't be scheduled – time is in the past.")
            (nextYear (cYear, (cMonth, (cDay, (cHour, (cMinute, cSecond))))))
    TDMask
      { year     = Just year'
      , month    = Just month'
      , day      = Just day'
      , hour     = Just hour'
      , minute   = Just minute'
      , second   = Just second'
      , weekdays = Nothing
      , timezone = Nothing
      }

main = do
  args <- getArgs
  if null args
    then listReminders
    else scheduleReminder args

scheduleReminder (dt:msg) = do
  tz <- loadLocalTZ
  time <- getCurrentTime
  -- Forcing evaluation because otherwise, if something fails, it'll fail
  -- during writing the reminder, and that'd be bad.
  let
    maskP    = momentP <* eof
    maskFunc = either (error . show) id $ parse maskP "" dt
  mask <- evaluate . force $ maskFunc tz time
  newUUID <- randomIO
  let reminder = Reminder
        { mask             = mask
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
