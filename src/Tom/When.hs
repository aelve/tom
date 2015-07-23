{-# LANGUAGE
RecordWildCards,
DeriveGeneric,
TupleSections,
FlexibleContexts
  #-}


module Tom.When
(
  When(..),
  allWhenParsers,
  momentP,
  wildcardP,
  durationP,
)
where


-- General
import Control.Applicative
import Control.Monad
import Data.Foldable (asum)
import Data.Maybe
-- Lists
import Data.List (find)
-- Parsing (Read)
import Text.Read (Read(..))
import qualified Text.Read as Read (lift)
import qualified Text.ParserCombinators.ReadP as ReadP hiding (optional)
import Text.ParserCombinators.ReadPrec (ReadPrec)
-- Parsing (Parsec); this one is used more and thus imported unqualified
import Text.Parsec hiding (optional, (<|>))
import Text.Parsec.String
-- Strictness
import Control.DeepSeq
-- Text
import Text.Printf
-- Generics (used to autoderive NFData)
import GHC.Generics
-- Time
import Data.Time
import Data.Time.Calendar.MonthDay
import Data.Time.Zones
-- Tom-specific
import Tom.Time


-- | A type for specifying moments of time when a reminder should fire.
data When
  = Mask {
      year     :: Maybe Integer,
      month    :: Maybe Int,
      day      :: Maybe Int,
      hour     :: Maybe Int,
      minute   :: Maybe Int,
      second   :: Maybe Int,
      weekdays :: Maybe [Int],    -- ^ Numbers between 1 and 7.
      timezone :: Maybe String }  -- ^ 'Nothing' = always use local timezone.
  deriving (Eq, Generic)

-- Examples of format used by Read and Show instances of Mask:
-- 
--   - xxxx-xx-03,13.xx:56
--   - 2015-xx-xx[6,7],12.00:00(UTC)

instance Show When where
  show Mask{..} = do
    -- Show something with padding. If not set, show some “x”s; if set, show
    -- and pad with zeroes.
    -- 
    -- >>> mb 2 Nothing
    -- "xx"
    -- 
    -- >>> mb 2 (Just 3)
    -- "03"
    let mb :: (Show a, Integral a) => Int -> Maybe a -> String
        mb n Nothing  = replicate n 'x'
        mb n (Just x) = let s = show x
                        in  replicate (n - length s) '0' ++ s
    printf "%s-%s-%s%s,%s.%s:%s%s"
      (mb 4 year) (mb 2 month) (mb 2 day)
      (maybe "" show weekdays)
      (mb 2 hour) (mb 2 minute) (mb 2 second)
      (maybe "" (\s -> "(" ++ olsonToTZName s ++ ")") timezone)

instance Read When where
  readPrec = do
    -- Some local definitions to make life easier (since Parsec steals
    -- ReadP's names).
    let lift       = Read.lift
        char       = ReadP.char
        between    = ReadP.between
        munch      = ReadP.munch
        string     = ReadP.string
        satisfy    = ReadP.satisfy
        skipSpaces = ReadP.skipSpaces
    -- Okay, here goes.
    lift skipSpaces
    let wild p = lift (some (char 'x') *> pure Nothing) <|> (Just <$> p)
    let nonnegative :: (Integral a, Read a) => ReadPrec a
        nonnegative = lift $ read <$> some (satisfy (`elem` ['0'..'9']))
    year  <- wild nonnegative <* lift (string "-")
    month <- wild nonnegative <* lift (string "-")
    day   <- wild nonnegative
    weekdays <- optional readPrec
    lift (string ",")
    hour   <- wild nonnegative <* lift (string ".")
    minute <- wild nonnegative <* lift (string ":")
    second <- wild nonnegative
    let parseTZName name = case tzNameToOlson name of
          Nothing -> fail ("unknown time zone name: '" ++ name ++ "'")
          Just tz -> return tz
    timezone <- lift . optional $
      parseTZName =<< between (char '(') (char ')') (munch (/= ')'))
    return Mask{..}

instance NFData When

-- | A parser for time specifiers ('When'). IO may be needed to query
-- timezones, for instance.
type WhenParser = Parser (IO When)

-- | All mask parsers exported by this module.
allWhenParsers :: [WhenParser]
allWhenParsers = [momentP, wildcardP, durationP]

-- | A parser for nonnegative integers (0, 1, 2, ...).
nonnegative :: (Read a, Integral a) => Parser a
nonnegative = read <$> some digit

-- | A parser for “am”/“pm”, returning the function to apply to hours to get
-- the corrected version.
parseAMPM :: (Num a, Ord a) => Parser (a -> a)
parseAMPM = do
  -- 1. “pm” means “add 12 to hour”.
  -- 2. Unless it's 12pm, in which case it doesn't.
  -- 3. And “am” can't be ignored, because 12am ≠ 12.00.
  -- 4. 13am shall mean 1.00. For consistency.
  let fromPM x = if x >= 12 then x else x + 12
      fromAM x = if x <  12 then x else x - 12
  choice [
    string "pm" *> pure fromPM,
    string "am" *> pure fromAM,
    pure id ]

{- |
Parses year, accounting for the “assume current millenium” shortcut.

  * @"10"@   → 2010
  * @"112"@  → 2112
  * @"2020"@ → 2020
  * @"3388"@ → 3388
-}
yearP :: Parser Integer
yearP = do
  s <- some digit
  return $ if length s < 4 then 2000 + read s else read s

-- | Timezone name parser. Returns already queried Olson name.
timezoneP :: Parser String
timezoneP = do
  name <- some (letter <|> digit <|> oneOf "-+")
  case tzNameToOlson name of
    Nothing -> mzero
    Just tz -> return tz

{- |
A parser for moments in time.

All numbers can be specified using any amount of digits. Date and time are
separated with a slash. Date can be omitted.

Date format:

  * Y-M-D
  * M-D
  * D

Time format: [H][.MM][:SS][am|pm][,timezone] (all components can be omitted).

The next suitable time is always chosen. For instance, if it's past 9pm
already, then “9pm” will resolve to 9pm of the next day. In the same way,
“2-29/8am” refers to morning of the next February 29 (which may happen in as
much as 4 years).

If the resulting time is always in the past, the function will fail.

A timezone can be specified as an abbreviation. At the moment, only a handful
of abbreviations are supported.
-}
momentP :: WhenParser
momentP = do
  -- Parsing date: it's either “Y-M-D”, “M-D”, “D”, or nothing (and then
  -- there is no slash). We use Just to denote that year/month/day is set.
  (mbYear, mbMonth, mbDay) <- choice $ map try
    [ do y <- yearP       <* string "-"
         m <- nonnegative <* string "-"
         d <- nonnegative <* string "/"
         return (Just y, Just m, Just d)
    , do m <- nonnegative <* string "-"
         d <- nonnegative <* string "/"
         return (Nothing, Just m, Just d)
    , do d <- nonnegative <* string "/"
         return (Nothing, Nothing, Just d)
    , return (Nothing, Nothing, Nothing)
    ]

  -- We parse [hour][.minute][:second][am|pm][,timezone], where every
  -- component is optional.
  (mbHour, mbMinute, mbSecond, mbTZ) <- do
    h <- optional nonnegative
    -- If hour is set and minute isn't, it's assumed to be 0, *not*
    -- omitted. For instance, “10am” really means “10.00:00”.
    m <- Just <$> (char '.' *> nonnegative)           <|>
         pure (if isJust h then Just 0 else Nothing)
    -- Same for minute/second.
    s <- Just <$> (char ':' *> nonnegative)           <|>
         pure (if isJust m then Just 0 else Nothing)
    -- “am”/“pm”. 
    fromAMPM <- parseAMPM
    -- Timezone.
    tz <- optional (string "," *> timezoneP)
    -- And now we can return parsed hour, minute, second, and timezone.
    return (fromAMPM <$> h, m, s, tz)

  -- Finally, we have to fill in the blanks (“Nothing”) so that the result is
  -- the *least* possible time which is still bigger than the current time.
  return $ do
    time <- getCurrentTime
    localTZ <- loadLocalTZ
    
    -- The parser returns a function which is given current time and
    -- timezone. We're inside this function now, and we turn given time and
    -- timezone into something more usable – namely, year/month/day/etc.
    tz <- case mbTZ of
      Nothing -> return localTZ
      Just x  -> loadSystemTZ x
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
    return Mask {
      year     = Just year',
      month    = Just month',
      day      = Just day',
      hour     = Just hour',
      minute   = Just minute',
      second   = Just second',
      weekdays = Nothing,
      timezone = mbTZ }

{- |
A parser for masks with wildcards.

All numbers can be specified using any amount of digits. Date and time are
separated with a slash. Date can be omitted.

Date format:

  * Y-M-D
  * M-D
  * D

Time format: [H][.MM][:SS][am|pm][,timezone] (all components can be omitted).

You can use any amount of “x”s instead of a number to specify that it can be
any number.

Omitted minute/second aren't assumed to be wildcards, so it's safe to do
“x.30” without it meaning “x.30:x”.

am/pm isn't really part of the mask, so “xpm” does not mean “the 2nd half of
the day”.

A timezone can be specified as an abbreviation. At the moment, only a handful
of abbreviations are supported.
-}
wildcardP :: WhenParser
wildcardP = do
  -- A function to turn any parser into a parser which accepts a wildcard
  -- (and returns Nothing in that case).
  let wild p = (some (char 'x') *> pure Nothing) <|> (Just <$> p)

  -- Parsing date: it's either “Y-M-D”, “M-D”, “D”, or nothing (and then
  -- there is no slash).
  (mbYear, mbMonth, mbDay) <- choice $ map try
    [ do y <- wild yearP       <* string "-"
         m <- wild nonnegative <* string "-"
         d <- wild nonnegative <* string "/"
         return (y, m, d)
    , do m <- wild nonnegative <* string "-"
         d <- wild nonnegative <* string "/"
         return (Nothing, m, d)
    , do d <- wild nonnegative <* string "/"
         return (Nothing, Nothing, d)
    , return (Nothing, Nothing, Nothing)
    ]

  -- [hour][.minute][:second][am|pm][,timezone]
  (mbHour, mbMinute, mbSecond, mbTZ) <- do
    h <- option Nothing (wild nonnegative)
    m <- option (Just 0) (char '.' *> wild nonnegative)
    s <- option (Just 0) (char ':' *> wild nonnegative)
    -- “am”/“pm”. 
    fromAMPM <- parseAMPM
    -- Timezone.
    tz <- optional (string "," *> timezoneP)
    -- And now we can return parsed hour, minute, second, and timezone.
    return (fromAMPM <$> h, m, s, tz)

  return $ return Mask {
    year     = mbYear,
    month    = mbMonth,
    day      = mbDay,
    hour     = mbHour,
    minute   = mbMinute,
    second   = mbSecond,
    weekdays = Nothing,
    timezone = mbTZ }

{- |
A parser for time durations (like “1h20m”).

A string it parses consists of a sequence of things, each being a number
followed by one of:

  * “h” (hours)
  * “m” (minutes)
  * “s” (seconds)
-}
durationP :: WhenParser
durationP = do
  let thingP = do
        n <- nonnegative
        choice [
          string "h" *> pure (n*3600),
          string "m" *> pure (n*60),
          string "s" *> pure n ]
  total <- sum <$> many1 thingP
  return $ do
    time <- getCurrentTime
    let ((cYear,cMonth,cDay),(cHour,cMinute,cSecond)) =
          expandTime utcTZ (addUTCTime (fromIntegral total) time)
    return Mask {
      year     = Just cYear,
      month    = Just cMonth,
      day      = Just cDay,
      hour     = Just cHour,
      minute   = Just cMinute,
      second   = Just cSecond,
      weekdays = Nothing,
      timezone = tzNameToOlson "UTC" }
