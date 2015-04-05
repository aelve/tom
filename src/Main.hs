{-# LANGUAGE
  ViewPatterns
, BangPatterns
, TupleSections
, MultiWayIf
  #-}


import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Control.Monad
import           Data.Foldable (asum, for_, traverse_)
import           Data.IORef
import           Data.List (find)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Time
import           Data.Time.Calendar.MonthDay
import           Data.Time.Zones
import           GHC.Exts (sortWith)
import           Graphics.UI.Gtk
import           System.Environment
import           System.Random
import           Text.Parsec hiding ((<|>), optional)
import           Text.Parsec.String

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
  newUUID <- randomIO
  let reminder = Reminder
        { mask             = mask
        , message          = unwords msg
        , lastSeen         = time
        , lastAcknowledged = time
        , uuid             = newUUID
        }
  withReminderFile $ \f -> do
    appendFile f (show reminder ++ "\n")
    putStrLn "Scheduled a reminder."

listReminders args = do
  withReminderFile $ \f -> do
    rs <- readReminders f
    let rs' = case args of
                []                 -> rs
                ["--sort", "ack"]  -> sortWith lastAcknowledged rs
                ["--sort", "seen"] -> sortWith lastSeen rs
    for_ rs' $ \r -> do
      putStrLn (show (mask r) ++ ": " ++ message r)

daemonMain = do
  alertsRef <- newIORef M.empty
  initGUI
  timeoutAdd (loop alertsRef >> return True) 1000  -- Repeat every 1s.
  mainGUI

loop alertsRef =
  withReminderFile $ \f -> do
    rs <- readReminders f
    t <- getCurrentTime
    tz <- loadLocalTZ

    -- We show a reminder if either holds:
    -- 
    -- a) It got expired since the moment it was seen.
    -- 
    -- b) It got expired since the moment it was last acknowledged, and it
    -- was seen more than 5min ago. So, it'll be shown every 5min if you keep
    -- not acknowledging it.
    let isExpired r = do
          reexpired <- reminderInInterval (lastSeen r) t (mask r)
          forgotten <- reminderInInterval (lastAcknowledged r) t (mask r)
          return $
            reexpired ||
            forgotten && diffUTCTime t (lastSeen r) >= 5*60

    expired <- filterM isExpired rs
    for_ expired $ \r -> do
      putStrLn $ "Reminder " ++ show (uuid r) ++ " has expired."

      -- If the old alert window is still hanging around, close it.
      mbOldDialog <- M.lookup (uuid r) <$> readIORef alertsRef
      traverse_ widgetDestroy mbOldDialog
      modifyIORef' alertsRef (M.delete (uuid r))

      -- Create another alert window.
      alert <- messageDialogNew
                 Nothing
                 []       -- flags
                 MessageInfo
                 ButtonsYesNo
                 ("Reminder: " ++ message r ++ "\n\n" ++
                  "(" ++ show (mask r) ++ ")")

      -- When we get a response, we just update “last seen” and “last
      -- acknowledged” fields and close the alert window.
      alert `on` response $ \rid -> do
        t <- getCurrentTime
        withReminderFile $ \f ->
          modifyReminder f (uuid r) $ \reminder ->
            if (rid == ResponseYes)
              then reminder { lastSeen         = t
                            , lastAcknowledged = t }
              else reminder { lastSeen         = t }
        widgetDestroy alert

      -- When the alert window is closed, we remove it from the map.
      alert `after` objectDestroy $ do
        modifyIORef' alertsRef (M.delete (uuid r))

      -- Add the alert to the map.
      modifyIORef' alertsRef (M.insert (uuid r) alert)

      -- Show the alert.
      widgetShow alert
      modifyReminder f (uuid r) $ \reminder -> reminder
        { lastSeen = t }
