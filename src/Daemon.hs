import Control.Applicative
import Data.Foldable (for_)
import System.FilePath
import System.Directory
import Data.Time
import Data.Fixed
import Control.Monad
import Data.List
import System.FileLock
import qualified System.IO.Strict as Strict
import Graphics.UI.Gtk

data Reminder = Reminder {
    time    :: UTCTime
  , message :: String
  }
  deriving (Eq, Read, Show)

withReminderFile action = do
  dir <- getDir
  withFileLock (dir </> "lock") Exclusive $ \_ -> do
    let fileName = (dir </> "reminders")
    ex <- doesFileExist fileName
    unless ex $
      writeFile fileName ""
    action fileName

getDir = do
  dir <- getAppUserDataDirectory "remind"
  ex <- doesDirectoryExist dir
  unless ex $
    createDirectory dir
  return dir

readReminders :: FilePath -> IO [Reminder]
readReminders f = map read . lines <$> Strict.readFile f

removeReminder r = withReminderFile $ \f -> do
  putStrLn $ "Removing reminder " ++ show r ++ "."
  rs <- readReminders f
  writeFile f (unlines (map show (delete r rs)))

main = do
  initGUI
  timeoutAdd (loop >> return True) 1000
  mainGUI

loop =
  withReminderFile $ \f -> do
    rs <- readReminders f
    t <- getCurrentTime
    let isExpired r = let diff = diffUTCTime t (time r)
                      in  diff >= 0 && diff `mod'` 60 <= 1
    let (expired, rest) = partition isExpired rs
    for_ expired $ \r -> do
      dialog <- messageDialogNew
                  Nothing
                  []       -- flags
                  MessageInfo
                  ButtonsYesNo
                  ("         Reminder: " ++ message r ++ "         ")
      dialog `on` response $ \rid -> do
        when (rid == ResponseYes) $
          removeReminder r
        widgetDestroy dialog
      widgetShow dialog
