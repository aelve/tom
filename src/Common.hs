{-# LANGUAGE
    RecordWildCards
  , ViewPatterns
  , DeriveGeneric
  #-}

module Common
(
  TDMask(..),
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
import Data.Tuple
import Data.List (delete)
import qualified System.IO.Strict as Strict
import Data.Fixed
import Data.Ix
import Data.Maybe
import Data.Char
import Data.UUID hiding (null)
import Control.DeepSeq
import GHC.Generics (Generic)
import Text.Printf
import Text.Read
import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.ReadP hiding (optional)

data TDMask = TDMask
  { year     :: Maybe Integer
  , month    :: Maybe Int
  , day      :: Maybe Int
  , hour     :: Maybe Int
  , minute   :: Maybe Int
  , second   :: Maybe Int
  , weekdays :: Maybe [Int]     -- ^ Numbers between 1 and 7.
  , timezone :: Maybe String    -- ^ 'Nothing' = current timezone is assumed.
  }
  deriving (Eq, Generic)

-- Examples of format used by Read and Show instances of TDMask:
-- 
--   - xxxx-xx-03,13.xx:56
--   - 2015-xx-xx[6,7],12.00:00(UTC)

instance Show TDMask where
  show TDMask{..} = do
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

instance Read TDMask where
  readPrec = do
    lift skipSpaces
    let wild p = lift (many1 (char 'x') *> pure Nothing) <|> (Just <$> p)
    let nonnegative :: (Integral a, Read a) => ReadPrec a
        nonnegative = lift $ read <$> many1 (satisfy (`elem` ['0'..'9']))
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
    timezone <- lift $ optional 
      (parseTZName =<< between (char '(') (char ')') (munch (/= ')')))
    return TDMask{..}

instance NFData TDMask

data Reminder = Reminder
  { mask             :: TDMask
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
    let fileName = (dir </> "reminders")
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

-- ((year, month, day), (hour, minute, second))
expandTime :: TZ -> UTCTime -> ((Integer, Int, Int), (Int, Int, Int))
expandTime tz t = ((year, month, day), (hour, minute, second))
  where
    local = utcToLocalTimeTZ tz t
    (year, month, day) = toGregorian (localDay local)
    TimeOfDay hour minute (truncate -> second) = localTimeOfDay local

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
  -> TDMask        -- ^ Reminder's time mask.
  -> IO Bool
reminderInInterval (ceilingUTCTime -> a) (floorUTCTime -> b) TDMask{..} = do
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

-- | Return Olson timezone name corresponding to an abbreviation.
tzNameToOlson :: String -> Maybe String
tzNameToOlson s = lookup (map toUpper s) tzAbbreviations

-- | Return abbreviation corresponding to Olson timezone name. Should never
-- be called on zone names which weren't produced by 'tzNameToOlson'.
olsonToTZName :: String -> String
olsonToTZName s = fromMaybe err $ lookup s (map swap tzAbbreviations)
  where
    err = error "olsonToTZName: can't find the name '" ++ s ++ "'"

-- | This list is optimised for common usage, not for precision; for
-- instance, “PST” means just “Pacific Time” (which means that in reality it
-- means “PDT” half of the time). So, all “standard” zones actually are
-- “standard/daylight”.
tzAbbreviations :: [(String, String)]
tzAbbreviations =
  -- universal time
  [ ( "UTC"    , "Etc/UTC" )
  , ( "UCT"    , "Etc/UTC" )
  , ( "GMT"    , "Etc/UTC" )
  -- Moscow Time
  , ( "MSK"    , "Europe/Moscow" )
  -- Pacific Time
  , ( "PT"     , "America/Los_Angeles" )
  , ( "PST"    , "America/Los_Angeles" )
  -- Mountain Time
  , ( "MT"     , "America/Phoenix" )
  , ( "MST"    , "America/Phoenix" )
  -- Central Time
  , ( "CT"     , "America/Chicago" )
  , ( "CST"    , "America/Chicago" )
  -- Eastern Time
  , ( "ET"     , "America/New_York" )
  , ( "EST"    , "America/New_York" )
  -- UTC-x
  , ( "UTC-0"  , "Etc/UTC"    )
  , ( "UTC-1"  , "Etc/GMT-1"  )
  , ( "UTC-2"  , "Etc/GMT-2"  )
  , ( "UTC-3"  , "Etc/GMT-3"  )
  , ( "UTC-4"  , "Etc/GMT-4"  )
  , ( "UTC-5"  , "Etc/GMT-5"  )
  , ( "UTC-6"  , "Etc/GMT-6"  )
  , ( "UTC-7"  , "Etc/GMT-7"  )
  , ( "UTC-8"  , "Etc/GMT-8"  )
  , ( "UTC-9"  , "Etc/GMT-9"  )
  , ( "UTC-10" , "Etc/GMT-10" )
  , ( "UTC-11" , "Etc/GMT-11" )
  , ( "UTC-12" , "Etc/GMT-12" )
  -- UTC+x
  , ( "UTC+0"  , "Etc/UTC"    )
  , ( "UTC+1"  , "Etc/GMT+1"  )
  , ( "UTC+2"  , "Etc/GMT+2"  )
  , ( "UTC+3"  , "Etc/GMT+3"  )
  , ( "UTC+4"  , "Etc/GMT+4"  )
  , ( "UTC+5"  , "Etc/GMT+5"  )
  , ( "UTC+6"  , "Etc/GMT+6"  )
  , ( "UTC+7"  , "Etc/GMT+7"  )
  , ( "UTC+8"  , "Etc/GMT+8"  )
  , ( "UTC+9"  , "Etc/GMT+9"  )
  , ( "UTC+10" , "Etc/GMT+10" )
  , ( "UTC+11" , "Etc/GMT+11" )
  , ( "UTC+12" , "Etc/GMT+12" )
  -- GMT-x
  , ( "GMT-0"  , "Etc/UTC"    )
  , ( "GMT-1"  , "Etc/GMT-1"  )
  , ( "GMT-2"  , "Etc/GMT-2"  )
  , ( "GMT-3"  , "Etc/GMT-3"  )
  , ( "GMT-4"  , "Etc/GMT-4"  )
  , ( "GMT-5"  , "Etc/GMT-5"  )
  , ( "GMT-6"  , "Etc/GMT-6"  )
  , ( "GMT-7"  , "Etc/GMT-7"  )
  , ( "GMT-8"  , "Etc/GMT-8"  )
  , ( "GMT-9"  , "Etc/GMT-9"  )
  , ( "GMT-10" , "Etc/GMT-10" )
  , ( "GMT-11" , "Etc/GMT-11" )
  , ( "GMT-12" , "Etc/GMT-12" )
  -- GMT+x
  , ( "GMT+0"  , "Etc/UTC"    )
  , ( "GMT+1"  , "Etc/GMT+1"  )
  , ( "GMT+2"  , "Etc/GMT+2"  )
  , ( "GMT+3"  , "Etc/GMT+3"  )
  , ( "GMT+4"  , "Etc/GMT+4"  )
  , ( "GMT+5"  , "Etc/GMT+5"  )
  , ( "GMT+6"  , "Etc/GMT+6"  )
  , ( "GMT+7"  , "Etc/GMT+7"  )
  , ( "GMT+8"  , "Etc/GMT+8"  )
  , ( "GMT+9"  , "Etc/GMT+9"  )
  , ( "GMT+10" , "Etc/GMT+10" )
  , ( "GMT+11" , "Etc/GMT+11" )
  , ( "GMT+12" , "Etc/GMT+12" )
  ]
