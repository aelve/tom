{-# LANGUAGE
ViewPatterns,
FlexibleContexts,
NoImplicitPrelude
  #-}


module Tom.Utils
(
  -- * Time
  tzNameToOlson,
  olsonToTZName,
  expandTime,
  floorUTCTime,
  ceilingUTCTime,
  -- * Absolute time
  getAbsoluteTime,
  utcToAbsoluteTime,
  absoluteTimeToUTC,
)
where


-- General
import BasePrelude hiding (second)
-- Monad transformers
import Control.Monad.Writer
-- Time
import Data.Time
import Data.Time.Zones
import Data.Time.Clock.TAI
import Data.Time.Clock.AnnouncedLeapSeconds    -- leapseconds-announced


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
tzAbbreviations = execWriter $ do
  -- (==>) means “add this pair to the list of pairs”. execWriter means
  -- “execute Writer to get a list of pairs”. Google “writer monad” for
  -- details, or just look at the documentation for “tell”.
  let a ==> b = tell [(a, b)]
  -- universal time
  "UTC" ==> "Etc/UTC"
  "UCT" ==> "Etc/UTC"
  "GMT" ==> "Etc/UTC"
  -- Moscow Time
  "MSK" ==> "Europe/Moscow"
  -- Pacific Time
  "PT"  ==> "America/Los_Angeles"
  "PST" ==> "America/Los_Angeles"
  -- Mountain Time
  "MT"  ==> "America/Phoenix"
  "MST" ==> "America/Phoenix"
  -- Central Time
  "CT"  ==> "America/Chicago"
  "CST" ==> "America/Chicago"
  -- Eastern Time
  "ET"  ==> "America/New_York"
  "EST" ==> "America/New_York"
  -- UTC-x, UTC+x, GMT-x, GMT+x (NB: signs in Olson database are the opposite
  -- of what would be expected – UTC-x is really “Etc/GMT+X”)
  "UTC-0" ==> "Etc/UTC"
  "UTC+0" ==> "Etc/UTC"
  "GMT-0" ==> "Etc/UTC"
  "GMT+0" ==> "Etc/UTC"
  for_ [1..12 :: Int] $ \x -> do
    let sx = show x
    ("UTC-" ++ sx) ==> ("Etc/GMT+" ++ sx)
    ("UTC+" ++ sx) ==> ("Etc/GMT-" ++ sx)
    ("GMT-" ++ sx) ==> ("Etc/GMT+" ++ sx)
    ("GMT+" ++ sx) ==> ("Etc/GMT-" ++ sx)

-- ((year, month, day), (hour, minute, second))
expandTime :: TZ -> UTCTime -> ((Integer, Int, Int), (Int, Int, Int))
expandTime tz t = ((year, month, day), (hour, minute, second))
  where
    local = utcToLocalTimeTZ tz t
    (year, month, day) = toGregorian (localDay local)
    TimeOfDay hour minute (truncate -> second) = localTimeOfDay local

taiToSeconds :: AbsoluteTime -> DiffTime
taiToSeconds t = diffAbsoluteTime t taiEpoch

secondsToTAI :: DiffTime -> AbsoluteTime
secondsToTAI t = addAbsoluteTime t taiEpoch

-- | Round 'UTCTime' down to have a whole number of seconds.
floorUTCTime :: UTCTime -> UTCTime
floorUTCTime = absoluteTimeToUTC . secondsToTAI
             . fromInteger . floor
             . taiToSeconds . utcToAbsoluteTime

-- | Round 'UTCTime' up to have a whole number of seconds.
ceilingUTCTime :: UTCTime -> UTCTime
ceilingUTCTime = absoluteTimeToUTC . secondsToTAI
               . fromInteger . ceiling
               . taiToSeconds . utcToAbsoluteTime

getAbsoluteTime :: IO AbsoluteTime
getAbsoluteTime = utcToAbsoluteTime <$> getCurrentTime

utcToAbsoluteTime :: UTCTime -> AbsoluteTime
utcToAbsoluteTime = utcToTAITime lst

absoluteTimeToUTC :: AbsoluteTime -> UTCTime
absoluteTimeToUTC = taiToUTCTime lst
