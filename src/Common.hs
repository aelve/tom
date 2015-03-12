module Common where

import Control.Applicative
import Control.Monad
import System.Directory
import System.FilePath
import System.FileLock
import Data.Time
import Data.List (delete)
import qualified System.IO.Strict as Strict

data Reminder = Reminder {
    time    :: UTCTime
  , message :: String
  }
  deriving (Eq, Read, Show)

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

-- | Has to be called in 'withReminderFile'.
readReminders :: FilePath -> IO [Reminder]
readReminders f = map read . lines <$> Strict.readFile f

removeReminder r = withReminderFile $ \f -> do
  putStrLn $ "Removing reminder " ++ show r ++ "."
  rs <- readReminders f
  writeFile f (unlines (map show (delete r rs)))
