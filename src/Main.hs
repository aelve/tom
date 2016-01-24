{-# LANGUAGE
NoImplicitPrelude
  #-}


module Main (main) where


-- General
import BasePrelude hiding (try)
-- Lenses
import Lens.Micro.Platform
-- Containers
import qualified Data.Map as M
-- Text
import Data.Text (Text)
import qualified Data.Text as T
-- Parsing
import Text.Megaparsec
-- Time
import Data.Time
-- acid-state
import Data.Acid
-- Tom-specific
import Tom.When
import Tom.Reminders
import Tom.Daemon
import Tom.GUI
import qualified Tom.RPC as RPC


main :: IO ()
main = do
  args <- getArgs
  case args of
    []                 -> runGUI
    ["--list"]         -> listReminders "created"
    ["--list", method] -> listReminders method
    ["--daemon"]       -> runDaemon
    (sch:msg)          -> scheduleReminder (T.pack sch) (T.pack (unwords msg))

scheduleReminder :: Text -> Text -> IO ()
scheduleReminder scheduleStr msg = do
  time <- getCurrentTime
  let scheduleP = choice $ map (\p -> try (p <* eof)) allWhenParsers
  _schedule <- case parse scheduleP "" scheduleStr of
    Left err -> error (show err)
    Right x  -> x
  res <- RPC.call $ RPC.AddReminder $ Reminder {
    _schedule         = _schedule,
    _message          = msg,
    _created          = time,
    _lastSeen         = time,
    _lastAcknowledged = time,
    _snoozedUntil     = time }
  case res of
    Left err -> putStrLn $ "Error: " ++ RPC.showError err
    Right _  -> putStrLn "Scheduled a reminder."

listReminders :: String -> IO ()
listReminders method = withDB $ \db -> do
  rsOn  <- query db GetRemindersOn
  rsOff <- query db GetRemindersOff
  let sorted = case method of
        "ack"     -> sortWith (view lastAcknowledged)
        "seen"    -> sortWith (view lastSeen)
        "created" -> sortWith (view created)
        _         -> error ("unknown sorting method: " ++ method)
  putStrLn "Off:"
  for_ (sorted (M.elems rsOff)) $ \reminder ->
    printf "  %s: %s\n" (show (reminder ^. schedule)) (reminder ^. message)
  putStrLn ""
  putStrLn "On:"
  for_ (sorted (M.elems rsOn)) $ \reminder ->
    printf "  %s: %s\n" (show (reminder ^. schedule)) (reminder ^. message)
