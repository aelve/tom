{-# LANGUAGE
  ViewPatterns
  #-}


module Tom.Time
(
  tzNameToOlson,
  olsonToTZName,
  expandTime,
)
where


-- General
import Data.Maybe
import Data.Tuple
-- Text
import Data.Char
-- Time
import Data.Time
import Data.Time.Zones


-- ((year, month, day), (hour, minute, second))
expandTime :: TZ -> UTCTime -> ((Integer, Int, Int), (Int, Int, Int))
expandTime tz t = ((year, month, day), (hour, minute, second))
  where
    local = utcToLocalTimeTZ tz t
    (year, month, day) = toGregorian (localDay local)
    TimeOfDay hour minute (truncate -> second) = localTimeOfDay local

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
  -- UTC-x (NB: signs in Olson database are the opposite of “what people
  -- expect”, yes)
  , ( "UTC-0"  , "Etc/UTC"    )
  , ( "UTC-1"  , "Etc/GMT+1"  )
  , ( "UTC-2"  , "Etc/GMT+2"  )
  , ( "UTC-3"  , "Etc/GMT+3"  )
  , ( "UTC-4"  , "Etc/GMT+4"  )
  , ( "UTC-5"  , "Etc/GMT+5"  )
  , ( "UTC-6"  , "Etc/GMT+6"  )
  , ( "UTC-7"  , "Etc/GMT+7"  )
  , ( "UTC-8"  , "Etc/GMT+8"  )
  , ( "UTC-9"  , "Etc/GMT+9"  )
  , ( "UTC-10" , "Etc/GMT+10" )
  , ( "UTC-11" , "Etc/GMT+11" )
  , ( "UTC-12" , "Etc/GMT+12" )
  -- UTC+x
  , ( "UTC+0"  , "Etc/UTC"    )
  , ( "UTC+1"  , "Etc/GMT-1"  )
  , ( "UTC+2"  , "Etc/GMT-2"  )
  , ( "UTC+3"  , "Etc/GMT-3"  )
  , ( "UTC+4"  , "Etc/GMT-4"  )
  , ( "UTC+5"  , "Etc/GMT-5"  )
  , ( "UTC+6"  , "Etc/GMT-6"  )
  , ( "UTC+7"  , "Etc/GMT-7"  )
  , ( "UTC+8"  , "Etc/GMT-8"  )
  , ( "UTC+9"  , "Etc/GMT-9"  )
  , ( "UTC+10" , "Etc/GMT-10" )
  , ( "UTC+11" , "Etc/GMT-11" )
  , ( "UTC+12" , "Etc/GMT-12" )
  -- GMT-x
  , ( "GMT-0"  , "Etc/UTC"    )
  , ( "GMT-1"  , "Etc/GMT+1"  )
  , ( "GMT-2"  , "Etc/GMT+2"  )
  , ( "GMT-3"  , "Etc/GMT+3"  )
  , ( "GMT-4"  , "Etc/GMT+4"  )
  , ( "GMT-5"  , "Etc/GMT+5"  )
  , ( "GMT-6"  , "Etc/GMT+6"  )
  , ( "GMT-7"  , "Etc/GMT+7"  )
  , ( "GMT-8"  , "Etc/GMT+8"  )
  , ( "GMT-9"  , "Etc/GMT+9"  )
  , ( "GMT-10" , "Etc/GMT+10" )
  , ( "GMT-11" , "Etc/GMT+11" )
  , ( "GMT-12" , "Etc/GMT+12" )
  -- GMT+x
  , ( "GMT+0"  , "Etc/UTC"    )
  , ( "GMT+1"  , "Etc/GMT-1"  )
  , ( "GMT+2"  , "Etc/GMT-2"  )
  , ( "GMT+3"  , "Etc/GMT-3"  )
  , ( "GMT+4"  , "Etc/GMT-4"  )
  , ( "GMT+5"  , "Etc/GMT-5"  )
  , ( "GMT+6"  , "Etc/GMT-6"  )
  , ( "GMT+7"  , "Etc/GMT-7"  )
  , ( "GMT+8"  , "Etc/GMT-8"  )
  , ( "GMT+9"  , "Etc/GMT-9"  )
  , ( "GMT+10" , "Etc/GMT-10" )
  , ( "GMT+11" , "Etc/GMT-11" )
  , ( "GMT+12" , "Etc/GMT-12" )
  ]
