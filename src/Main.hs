{-# LANGUAGE
MultiWayIf
  #-}


-- General
import Control.Applicative
import Data.Foldable
-- Lenses
import Lens.Micro.Platform
-- Lists
import GHC.Exts (sortWith)
-- Parsing
import Text.Megaparsec
-- Text
import Text.Printf
-- Time
import Data.Time
-- Strictness
import Control.DeepSeq
import Control.Exception (evaluate)
-- IO
import System.Environment
-- Tom-specific
import Tom.When
import Tom.Reminders
import Tom.Daemon
import Tom.GUI


main :: IO ()
main = do
  args <- getArgs
  if | null args               -> runGUI
     | head args == "--sort"   -> listReminders args
     | head args == "--daemon" -> runDaemon
     | otherwise               -> scheduleReminder args

scheduleReminder :: [String] -> IO ()
scheduleReminder (dt:msg) = do
  time <- getCurrentTime
  -- Forcing evaluation because otherwise, if something fails, it'll fail
  -- during writing the reminder, and that'd be bad.
  let scheduleP = choice $ map (\p -> try (p <* eof)) allWhenParsers
  _schedule <- evaluate . force =<<
    either (error . show) id (parse scheduleP "" dt)
  let reminder = Reminder {
        _schedule         = _schedule,
        _message          = unwords msg,
        _created          = time,
        _lastSeen         = time,
        _lastAcknowledged = time,
        _snoozedUntil     = time }
  addReminder reminder
  putStrLn "Scheduled a reminder."

listReminders :: [String] -> IO ()
listReminders args = do
  file <- readRemindersFile
  let sorted = case args of
        ["--sort", "ack"]  -> sortWith (view lastAcknowledged)
        ["--sort", "seen"] -> sortWith (view lastSeen)
        []                 -> id  -- no need to sort reminders
  putStrLn "Off:"
  for_ (sorted (file ^.. remindersOff . each)) $ \reminder ->
    printf "  %s: %s\n" (show (reminder ^. schedule)) (reminder ^. message)
  putStrLn ""
  putStrLn "On:"
  for_ (sorted (file ^.. remindersOn . each)) $ \reminder ->
    printf "  %s: %s\n" (show (reminder ^. schedule)) (reminder ^. message)
