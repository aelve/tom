{-# LANGUAGE
  ViewPatterns
, BangPatterns
, TupleSections
, MultiWayIf
  #-}


-- General
import           Control.Applicative
import           Control.Monad
import           Data.Foldable (asum, for_, traverse_)
import           Data.Maybe
-- Parsing
import           Text.Parsec hiding ((<|>), optional)
import           Text.Parsec.String
-- Lists
import           Data.List (find)
import           GHC.Exts (sortWith)
-- Text
import           Text.Printf
-- Map
import qualified Data.Map as M
-- Time
import           Data.Time
import           Data.Time.Calendar.MonthDay
import           Data.Time.Zones
-- Strictness
import           Control.DeepSeq
import           Control.Exception (evaluate)
-- GTK
import           Graphics.UI.Gtk
-- IO
import           Data.IORef
import           System.Environment
-- Tom-specific
import           Tom.Mask
import           Tom.Common


main = do
  args <- getArgs
  if | null args || head args == "--sort" -> listReminders args
     | head args == "--daemon"            -> daemonMain
     | otherwise                          -> scheduleReminder args

scheduleReminder (dt:msg) = do
  time <- getCurrentTime
  -- Forcing evaluation because otherwise, if something fails, it'll fail
  -- during writing the reminder, and that'd be bad.
  let maskP = choice . map (\p -> try (p <* eof)) $ [momentP, wildcardP]
  mask <- evaluate . force =<<
            either (error . show) id (parse maskP "" dt)
  let reminder = Reminder
        { mask             = mask
        , message          = unwords msg
        , lastSeen         = time
        , lastAcknowledged = time
        , ignoreUntil      = time
        }
  withRemindersFile $ addReminder reminder
  putStrLn "Scheduled a reminder."

listReminders args = do
  file <- readRemindersFile 
  let sortRs = case args of
        []                 -> id
        ["--sort", "ack"]  -> sortWith lastAcknowledged
        ["--sort", "seen"] -> sortWith lastSeen
  putStrLn "Off:"
  for_ (sortRs (M.elems (remindersOff file))) $ \r ->
    printf "  %s: %s\n" (show (mask r)) (message r)
  putStrLn ""
  putStrLn "On:"
  for_ (sortRs (M.elems (remindersOn file))) $ \r ->
    printf "  %s: %s\n" (show (mask r)) (message r)

daemonMain = do
  alertsRef <- newIORef M.empty
  initGUI
  timeoutAdd (loop alertsRef >> return True) 1000  -- Repeat every 1s.
  mainGUI

loop alertsRef =
  withRemindersFile $ \file -> do
    t <- getCurrentTime
    tz <- loadLocalTZ

    -- We want the following behavior:
    -- 
    -- + When a reminder is shown and ignored, it's shown again in 5min
    -- (unless it gets expired again earlier than that – e.g. if it is set to
    -- be shown every minute).
    -- 
    -- + When a reminder is shown and snoozed, it's not shown again until the
    -- snooze interval ends, even if it gets expired again in that period.
    -- 
    -- So, we show a reminder if it's not snoozed and if either holds:
    -- 
    -- + It got expired since the moment it was seen.
    -- 
    -- + It got expired since the moment it was last acknowledged, and it was
    -- seen more than 5min ago.
    let isExpired r = do
          reexpired <- reminderInInterval (lastSeen r) t (mask r)
          forgotten <- reminderInInterval (lastAcknowledged r) t (mask r)
          return $
            and [ t >= ignoreUntil r
                , or [ reexpired
                     , forgotten && diffUTCTime t (lastSeen r) >= 5*60 ] ]

    expired <- filterM (isExpired . snd) (M.assocs (remindersOn file))
    for_ expired $ \(uuid, reminder) -> do
      putStrLn $ "Reminder " ++ show uuid ++ " has expired."

      -- If the old alert window is still hanging around, close it.
      mbOldDialog <- M.lookup uuid <$> readIORef alertsRef
      traverse_ widgetDestroy mbOldDialog
      modifyIORef' alertsRef (M.delete uuid)

      -- Create another alert window.
      alert <- messageDialogNew
                 Nothing
                 []       -- flags
                 MessageInfo
                 ButtonsYesNo
                 ("Reminder: " ++ message reminder ++ "\n\n" ++
                  "(" ++ show (mask reminder) ++ ")")

      -- Processing a response goes as follows:
      -- 
      -- + lastSeen is updated.
      -- + On “yes” lastAcknowledged is updated.
      -- + On “no” the reminder is snoozed for 5min.
      -- + The alert window is closed.
      alert `on` response $ \rid -> do
        t <- getCurrentTime
        withRemindersFile . fmap return $
          modifyReminder uuid $ \reminder ->
            if (rid == ResponseYes)
              then reminder { lastSeen         = t
                            , lastAcknowledged = t }
              else reminder { lastSeen         = t
                            , ignoreUntil      = addUTCTime (5*60) t }
        widgetDestroy alert

      -- When the alert window is closed, we remove it from the map.
      alert `after` objectDestroy $ do
        modifyIORef' alertsRef (M.delete uuid)

      -- Add the alert to the map.
      modifyIORef' alertsRef (M.insert uuid alert)

      -- Show the alert.
      widgetShow alert

    -- Finally, lastSeen of all snown reminders must be updated.
    let expired' = fmap (\r -> r { lastSeen = t }) (M.fromList expired)
    return file { remindersOn = M.union expired' (remindersOn file) }
