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
      (maybe "" (\s -> "(" ++ s ++ ")") timezone)

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
    timezone <- lift $
      optional $ between (char '(') (char ')') (munch (/= ')'))
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

tzAbbreviations :: [(String, String)]
tzAbbreviations =
  [ ("UTC", "UTC")
  , ("UCT", "UCT")
  , ("GMT", "GMT")
  , ("EET", "EET")
  , ("MSK", "Europe/Moscow")
  , ("VLAT", "Asia/Vladivostok")
  , ("OMST", "Asia/Omsk")
  , ("KRAT", "Asia/Krasnoyarsk")
  , ("YAKT", "Asia/Yakutsk")
  ]

{-
A list of time zones, taken from Wikipedia.
All of these are unavailable in tzAbbreviations yet.

ACDT 	Australian Central Daylight Savings Time 	UTC+10:30
ACST 	Australian Central Standard Time 	UTC+09:30
ACT 	Acre Time 	UTC−05
ACT 	ASEAN Common Time 	UTC+08
ADT 	Atlantic Daylight Time 	UTC−03
AEDT 	Australian Eastern Daylight Savings Time 	UTC+11
AEST 	Australian Eastern Standard Time 	UTC+10
AFT 	Afghanistan Time 	UTC+04:30
AKDT 	Alaska Daylight Time 	UTC−08
AKST 	Alaska Standard Time 	UTC−09
AMST 	Amazon Summer Time (Brazil)[1] 	UTC−03
AMST 	Armenia Summer Time 	UTC+05
AMT 	Amazon Time (Brazil)[2] 	UTC−04
AMT 	Armenia Time 	UTC+04
ART 	Argentina Time 	UTC−03
AST 	Arabia Standard Time 	UTC+03
AST 	Atlantic Standard Time 	UTC−04
AWDT 	Australian Western Daylight Time 	UTC+09
AWST 	Australian Western Standard Time 	UTC+08
AZOST 	Azores Standard Time 	UTC−01
AZT 	Azerbaijan Time 	UTC+04
BDT 	Brunei Time 	UTC+08
BIOT 	British Indian Ocean Time 	UTC+06
BIT 	Baker Island Time 	UTC−12
BOT 	Bolivia Time 	UTC−04
BRST 	Brasilia Summer Time 	UTC−02
BRT 	Brasilia Time 	UTC−03
BST 	Bangladesh Standard Time 	UTC+06
BST 	British Summer Time (British Standard Time from Feb 1968 to Oct 1971) 	UTC+01
BTT 	Bhutan Time 	UTC+06
CAT 	Central Africa Time 	UTC+02
CCT 	Cocos Islands Time 	UTC+06:30
CDT 	Central Daylight Time (North America) 	UTC−05
CDT 	Cuba Daylight Time[3] 	UTC−04
CEDT 	Central European Daylight Time 	UTC+02
CEST 	Central European Summer Time (Cf. HAEC) 	UTC+02
CET 	Central European Time 	UTC+01
CHADT 	Chatham Daylight Time 	UTC+13:45
CHAST 	Chatham Standard Time 	UTC+12:45
CHOT 	Choibalsan 	UTC+08
ChST 	Chamorro Standard Time 	UTC+10
CHUT 	Chuuk Time 	UTC+10
CIST 	Clipperton Island Standard Time 	UTC−08
CIT 	Central Indonesia Time 	UTC+08
CKT 	Cook Island Time 	UTC−10
CLST 	Chile Summer Time 	UTC−03
CLT 	Chile Standard Time 	UTC−04
COST 	Colombia Summer Time 	UTC−04
COT 	Colombia Time 	UTC−05
CST 	Central Standard Time (North America) 	UTC−06
CST 	China Standard Time 	UTC+08
CST 	Central Standard Time (Australia) 	UTC+09:30
CST 	Central Summer Time (Australia) 	UTC+10:30
CST 	Cuba Standard Time 	UTC−05
CT 	China time 	UTC+08
CVT 	Cape Verde Time 	UTC−01
CWST 	Central Western Standard Time (Australia) unofficial 	UTC+08:45
CXT 	Christmas Island Time 	UTC+07
DAVT 	Davis Time 	UTC+07
DDUT 	Dumont d'Urville Time 	UTC+10
DFT 	AIX specific equivalent of Central European Time[4] 	UTC+01
EASST 	Easter Island Standard Summer Time 	UTC−05
EAST 	Easter Island Standard Time 	UTC−06
EAT 	East Africa Time 	UTC+03
ECT 	Eastern Caribbean Time (does not recognise DST) 	UTC−04
ECT 	Ecuador Time 	UTC−05
EDT 	Eastern Daylight Time (North America) 	UTC−04
EEDT 	Eastern European Daylight Time 	UTC+03
EEST 	Eastern European Summer Time 	UTC+03
EGST 	Eastern Greenland Summer Time 	UTC+00
EGT 	Eastern Greenland Time 	UTC−01
EIT 	Eastern Indonesian Time 	UTC+09
EST 	Eastern Standard Time (North America) 	UTC−05
EST 	Eastern Standard Time (Australia) 	UTC+10
FET 	Further-eastern European Time 	UTC+03
FJT 	Fiji Time 	UTC+12
FKST 	Falkland Islands Standard Time 	UTC−03
FKST 	Falkland Islands Summer Time 	UTC−03
FKT 	Falkland Islands Time 	UTC−04
FNT 	Fernando de Noronha Time 	UTC−02
GALT 	Galapagos Time 	UTC−06
GAMT 	Gambier Islands 	UTC−09
GET 	Georgia Standard Time 	UTC+04
GFT 	French Guiana Time 	UTC−03
GILT 	Gilbert Island Time 	UTC+12
GIT 	Gambier Island Time 	UTC−09
GST 	South Georgia and the South Sandwich Islands 	UTC−02
GST 	Gulf Standard Time 	UTC+04
GYT 	Guyana Time 	UTC−04
HADT 	Hawaii-Aleutian Daylight Time 	UTC−09
HAEC 	Heure Avancée d'Europe Centrale francised name for CEST 	UTC+02
HAST 	Hawaii-Aleutian Standard Time 	UTC−10
HKT 	Hong Kong Time 	UTC+08
HMT 	Heard and McDonald Islands Time 	UTC+05
HOVT 	Khovd Time 	UTC+07
HST 	Hawaii Standard Time 	UTC−10
ICT 	Indochina Time 	UTC+07
IDT 	Israel Daylight Time 	UTC+03
IOT 	Indian Ocean Time 	UTC+03
IRDT 	Iran Daylight Time 	UTC+04:30
IRKT 	Irkutsk Time 	UTC+08
IRST 	Iran Standard Time 	UTC+03:30
IST 	Indian Standard Time 	UTC+05:30
IST 	Irish Standard Time[5] 	UTC+01
IST 	Israel Standard Time 	UTC+02
JST 	Japan Standard Time 	UTC+09
KGT 	Kyrgyzstan time 	UTC+06
KOST 	Kosrae Time 	UTC+11
KST 	Korea Standard Time 	UTC+09
LHST 	Lord Howe Standard Time 	UTC+10:30
LHST 	Lord Howe Summer Time 	UTC+11
LINT 	Line Islands Time 	UTC+14
MAGT 	Magadan Time 	UTC+12
MART 	Marquesas Islands Time 	UTC−09:30
MAWT 	Mawson Station Time 	UTC+05
MDT 	Mountain Daylight Time (North America) 	UTC−06
MET 	Middle European Time Same zone as CET 	UTC+01
MEST 	Middle European Saving Time Same zone as CEST 	UTC+02
MHT 	Marshall Islands 	UTC+12
MIST 	Macquarie Island Station Time 	UTC+11
MIT 	Marquesas Islands Time 	UTC−09:30
MMT 	Myanmar Time 	UTC+06:30
MST 	Malaysia Standard Time 	UTC+08
MST 	Mountain Standard Time (North America) 	UTC−07
MST 	Myanmar Standard Time 	UTC+06:30
MUT 	Mauritius Time 	UTC+04
MVT 	Maldives Time 	UTC+05
MYT 	Malaysia Time 	UTC+08
NCT 	New Caledonia Time 	UTC+11
NDT 	Newfoundland Daylight Time 	UTC−02:30
NFT 	Norfolk Time 	UTC+11:30
NPT 	Nepal Time 	UTC+05:45
NST 	Newfoundland Standard Time 	UTC−03:30
NT 	Newfoundland Time 	UTC−03:30
NUT 	Niue Time 	UTC−11
NZDT 	New Zealand Daylight Time 	UTC+13
NZST 	New Zealand Standard Time 	UTC+12
ORAT 	Oral Time 	UTC+05
PDT 	Pacific Daylight Time (North America) 	UTC−07
PET 	Peru Time 	UTC−05
PETT 	Kamchatka Time 	UTC+12
PGT 	Papua New Guinea Time 	UTC+10
PHOT 	Phoenix Island Time 	UTC+13
PKT 	Pakistan Standard Time 	UTC+05
PMDT 	Saint Pierre and Miquelon Daylight time 	UTC−02
PMST 	Saint Pierre and Miquelon Standard Time 	UTC−03
PONT 	Pohnpei Standard Time 	UTC+11
PST 	Pacific Standard Time (North America) 	UTC−08
PST 	Philippine Standard Time 	UTC+08
PYST 	Paraguay Summer Time (South America)[6] 	UTC−03
PYT 	Paraguay Time (South America)[7] 	UTC−04
RET 	Réunion Time 	UTC+04
ROTT 	Rothera Research Station Time 	UTC−03
SAKT 	Sakhalin Island time 	UTC+11
SAMT 	Samara Time 	UTC+04
SAST 	South African Standard Time 	UTC+02
SBT 	Solomon Islands Time 	UTC+11
SCT 	Seychelles Time 	UTC+04
SGT 	Singapore Time 	UTC+08
SLST 	Sri Lanka Time 	UTC+05:30
SRET 	Srednekolymsk Time 	UTC+11
SRT 	Suriname Time 	UTC−03
SST 	Samoa Standard Time 	UTC−11
SST 	Singapore Standard Time 	UTC+08
SYOT 	Showa Station Time 	UTC+03
TAHT 	Tahiti Time 	UTC−10
THA 	Thailand Standard Time 	UTC+07
TFT 	Indian/Kerguelen 	UTC+05
TJT 	Tajikistan Time 	UTC+05
TKT 	Tokelau Time 	UTC+13
TLT 	Timor Leste Time 	UTC+09
TMT 	Turkmenistan Time 	UTC+05
TOT 	Tonga Time 	UTC+13
TVT 	Tuvalu Time 	UTC+12
ULAT 	Ulaanbaatar Time 	UTC+08
USZ1 	Kaliningrad Time 	UTC+02
UYST 	Uruguay Summer Time 	UTC−02
UYT 	Uruguay Standard Time 	UTC−03
UZT 	Uzbekistan Time 	UTC+05
VET 	Venezuelan Standard Time 	UTC−04:30
VOLT 	Volgograd Time 	UTC+04
VOST 	Vostok Station Time 	UTC+06
VUT 	Vanuatu Time 	UTC+11
WAKT 	Wake Island Time 	UTC+12
WAST 	West Africa Summer Time 	UTC+02
WAT 	West Africa Time 	UTC+01
WEDT 	Western European Daylight Time 	UTC+01
WEST 	Western European Summer Time 	UTC+01
WET 	Western European Time 	UTC
WIT 	Western Indonesian Time 	UTC+07
WST 	Western Standard Time 	UTC+08
YEKT 	Yekaterinburg Time 	UTC+05
Z 	Zulu Time (Coordinated Universal Time) 	UTC
-}
