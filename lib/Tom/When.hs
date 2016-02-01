{-# LANGUAGE
RecordWildCards,
DeriveGeneric,
DeriveAnyClass,
TupleSections,
FlexibleContexts,
TemplateHaskell,
ViewPatterns,
ScopedTypeVariables,
TypeFamilies,
NoImplicitPrelude
  #-}


module Tom.When
(
  When(..),
  WhenParser,
  WhenParserData(..),
  getWhenParserData,
  runWhenParser,
  runWhenParser',
  parseWhen,
  parseWhen',
  -- * Individual parsers
  momentP,
  wildcardP,
  durationMomentP,
  periodicP,
)
where


-- General
import BasePrelude hiding (try, second)
-- Lenses
import Lens.Micro.Platform
-- Monads & monad transformers
import Control.Monad.Reader
-- Text
import Data.Text (Text)
-- Parsing
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.Lexer
-- Time
import Data.Time
import Data.Time.Clock.TAI
import Data.Time.Calendar.MonthDay
import Data.Time.Zones                         -- tz
-- acid-state & safecopy
import Data.SafeCopy
-- Binary
import Data.Binary
import Data.Binary.Orphans ()                  -- binary-orphans
-- Tom-specific
import Tom.Utils


-- | A type for specifying moments of time when a reminder should fire.
data When
  = Mask {
      year     :: Maybe Integer,
      month    :: Maybe Int,
      day      :: Maybe Int,
      hour     :: Maybe Int,
      minute   :: Maybe Int,
      second   :: Maybe Int,
      weekdays :: Maybe [Int],    -- ^ Numbers between 1 and 7
      timezone :: Maybe String }  -- ^ 'Nothing' = always use local timezone;
                                  --   otherwise the timezone is stored
                                  --   as a string in Olson format (e.g.
                                  --   “Europe/Paris”)
  | Moment {
      moment   :: AbsoluteTime }
  | Periodic {
      start    :: AbsoluteTime,   -- ^ 1st time the reminder should fire
      period   :: DiffTime }      -- ^ Period
  deriving (Eq, Generic, Binary)

data When_v0
  = Mask_v0 {
      year_v0     :: Maybe Integer,
      month_v0    :: Maybe Int,
      day_v0      :: Maybe Int,
      hour_v0     :: Maybe Int,
      minute_v0   :: Maybe Int,
      second_v0   :: Maybe Int,
      weekdays_v0 :: Maybe [Int],
      timezone_v0 :: Maybe String }
  | Moment_v0 {
      moment_v0   :: AbsoluteTime }

deriveSafeCopy 0 'base ''When_v0
deriveSafeCopy 1 'extension ''When

instance Migrate When where
  type MigrateFrom When = When_v0
  migrate Mask_v0{..} = Mask {
    year     = year_v0,
    month    = month_v0,
    day      = day_v0,
    hour     = hour_v0,
    minute   = minute_v0,
    second   = second_v0,
    weekdays = weekdays_v0,
    timezone = timezone_v0 }
  migrate Moment_v0{..} = Moment {
    moment = moment_v0 }

{-
Examples of format used by the Show instance of Mask:

  * xxxx-xx-03,13.xx:56
  * 2015-xx-xx[6,7],12.00:00(UTC)

TODO: document other constructors
-}

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
  show Moment{..} = "moment " ++ showAbsoluteTime moment
  show Periodic{..} = printf "every %s from %s"
                             (showDiffTime period) (showAbsoluteTime start)

showAbsoluteTime :: AbsoluteTime -> String
showAbsoluteTime = ("TAI " ++) . show .
                   utcToLocalTime utc . taiToUTCTime (const 0)

showDiffTime :: DiffTime -> String
showDiffTime (realToFrac -> seconds_) = do
  let hours, minutes :: Integer
      seconds :: Double
      (minutes_, seconds) = divMod' seconds_ 60
      (hours,    minutes) = divMod' minutes_ 60
  case properFraction seconds of
    (s :: Integer, 0) -> printf "%dh%dm%ds" hours minutes s
    _                 -> printf "%dh%dm%.3fs" hours minutes seconds

{- |
A parser for time specifiers ('When').

The parser is given access to the current time, timezone, and IO (might be needed to query some timezone from the timezone data file – look for 'loadSystemTZ').
-}
type WhenParser = ParsecT Text (ReaderT WhenParserData IO) When

data WhenParserData = WhenParserData {
  _currentTime :: UTCTime,
  _localTZ     :: TZ }

makeLenses ''WhenParserData

getWhenParserData :: IO WhenParserData
getWhenParserData = WhenParserData <$> getCurrentTime <*> loadLocalTZ

runWhenParser' :: WhenParserData -> WhenParser -> Text -> IO (Either ParseError When)
runWhenParser' pData p s = do
  runReaderT (runParserT (p <* eof) "" s) pData

runWhenParser :: WhenParser -> Text -> IO (Either ParseError When)
runWhenParser p s = do
  pData <- getWhenParserData
  runWhenParser' pData p s

parseWhen' :: WhenParserData -> Text -> IO (Either ParseError When)
parseWhen' pData = runWhenParser' pData p
  where
    p = choice $ map (\x -> try (x <* eof))
          [momentP, wildcardP, durationMomentP, periodicP]

parseWhen :: Text -> IO (Either ParseError When)
parseWhen s = do
  pData <- getWhenParserData
  parseWhen' pData s

-- | A parser for “am”/“pm”, returning the function to apply to hours to get
-- the corrected version.
parseAMPM :: (MonadParsec s m Char, Num a, Ord a) => m (a -> a)
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
yearP :: MonadParsec s m Char => m Integer
yearP = do
  s <- some digitChar
  return $ if length s < 4 then 2000 + read s else read s

-- | Timezone name parser. Returns already queried Olson name.
timezoneP :: MonadParsec s m Char => m String
timezoneP = do
  name <- some (letterChar <|> digitChar <|> oneOf "-+")
  case tzNameToOlson name of
    Nothing -> fail $ printf "unknown timezone name ‘%s’" name
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
    m <- option (if isJust h then Just 0 else Nothing)
           (Just <$> (char '.' *> nonnegative))
    -- Same for minute/second.
    s <- option (if isJust m then Just 0 else Nothing)
           (Just <$> (char ':' *> nonnegative))
    -- “am”/“pm”. 
    fromAMPM <- parseAMPM
    -- Timezone.
    tz <- optional (string "," *> timezoneP)
    -- And now we can return parsed hour, minute, second, and timezone.
    return (fromAMPM <$> h, m, s, tz)

  -- Finally, we have to fill in the blanks (“Nothing”) so that the result is
  -- the *least* possible time which is still bigger than the current time.

  -- Let's turn time and timezone into something more usable – namely,
  -- year/month/day/etc.
  time <- view currentTime
  tz <- case mbTZ of
    Nothing -> view localTZ
    Just x  -> liftIO $ loadSystemTZ x
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
          | x < y     -> Just (y, zeroSecond)
          | x == y    -> (y,) <$> nextSecond rest
          | otherwise -> Nothing
        -- If we don't have to use mbMinute, we try to increment the second
        -- first, and increase the minute if we can't increment the second.
        Nothing       -> (x,) <$> nextSecond rest  <|>
                         (guard (x <= 58) >> return (x+1, zeroSecond))

  -- nextHour is the same as nextMinute.
  let nextHour (x, rest) = case mbHour of
        Just y
          | x < y     -> Just (y, zeroMinute)
          | x == y    -> (y,) <$> nextMinute rest
          | otherwise -> Nothing
        Nothing       -> (x,) <$> nextMinute rest  <|>
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
            | x < y     -> Just (y, zeroHour)
            | x == y    -> (y,) <$> nextHour rest
            | otherwise -> Nothing
          Nothing       -> (x,) <$> nextHour rest  <|>
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
          | x < y     -> Just (y, zeroDay)
          | x == y    -> (y,) <$> nextDay (monthLength isLeap y) rest
          | otherwise -> Nothing
        Nothing       -> asum
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
          | x < y     -> Just (y, zeroMonth)
          | x == y    -> (y,) <$> nextMonth (isLeapYear y) rest
          | otherwise -> Nothing
        Nothing       -> asum
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
  (year', (month', (day', (hour', (minute', second'))))) <-
    -- TODO: figure out when this actually happens and add to tests
    case nextYear (cYear, (cMonth, (cDay, (cHour, (cMinute, cSecond))))) of
      Nothing -> fail "time is always in the past"
      Just x  -> return x
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

  return Mask {
    year     = mbYear,
    month    = mbMonth,
    day      = mbDay,
    hour     = mbHour,
    minute   = mbMinute,
    second   = mbSecond,
    weekdays = Nothing,
    timezone = mbTZ }

{- |
A parser for moments coming after some time (like “1h20m”).
-}
durationMomentP :: WhenParser
durationMomentP = do
  dur <- duration
  time <- view currentTime
  return $ Moment {
    moment = addAbsoluteTime dur (utcToAbsoluteTime time) }

{- |
A parser for periodic stuff: “every 1h20m”.
-}
periodicP :: WhenParser
periodicP = do
  string "every"
  skipSome spaceChar
  dur <- duration
  time <- view currentTime
  return $ Periodic {
    start  = addAbsoluteTime dur (utcToAbsoluteTime time),
    period = dur }

nonnegative :: (MonadParsec s m Char, Integral a) => m a
nonnegative = fromInteger <$> integer

{- |
'duration' parses strings consisting of things, each being a number followed by one of:

  * “h” (hours)
  * “m” (minutes)
  * “s” (seconds)
-}
duration :: MonadParsec s m Char => m DiffTime
duration = fmap (fromInteger . sum) $ some $ do
  n <- nonnegative
  choice [
    string "h" *> pure (n*3600),
    string "m" *> pure (n*60),
    string "s" *> pure n ]
