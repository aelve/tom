module Main (main) where


-- General
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
  case args of
    []                 -> runGUI
    ["--list"]         -> listReminders "created"
    ["--list", method] -> listReminders method
    ["--daemon"]       -> runDaemon
    (sch:msg)          -> scheduleReminder sch (unwords msg)

scheduleReminder :: String -> String -> IO ()
scheduleReminder scheduleStr msg = do
  time <- getCurrentTime
  -- Forcing evaluation because otherwise, if something fails, it'll fail
  -- during writing the reminder, and that'd be bad.
  let scheduleP = choice $ map (\p -> try (p <* eof)) allWhenParsers
  _schedule <- evaluate . force =<<
    either (error . show) id (parse scheduleP "" scheduleStr)
  addReminder $ Reminder {
    _schedule         = _schedule,
    _message          = msg,
    _created          = time,
    _lastSeen         = time,
    _lastAcknowledged = time,
    _snoozedUntil     = time }
  putStrLn "Scheduled a reminder."

listReminders :: String -> IO ()
listReminders method = do
  file <- readRemindersFile
  let sorted = case method of
        "ack"     -> sortWith (view lastAcknowledged)
        "seen"    -> sortWith (view lastSeen)
        "created" -> sortWith (view created)
        _         -> error ("unknown sorting method: " ++ method)
  putStrLn "Off:"
  for_ (sorted (file ^.. remindersOff . each)) $ \reminder ->
    printf "  %s: %s\n" (show (reminder ^. schedule)) (reminder ^. message)
  putStrLn ""
  putStrLn "On:"
  for_ (sorted (file ^.. remindersOn . each)) $ \reminder ->
    printf "  %s: %s\n" (show (reminder ^. schedule)) (reminder ^. message)
