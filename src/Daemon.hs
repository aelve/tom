import Control.Applicative
import Data.Foldable (for_, traverse_)
import Data.Time
import Data.Time.Zones
import Data.Fixed
import Control.Monad
import Data.List
import Graphics.UI.Gtk
import Data.IORef
import qualified Data.Map as M
import Debug.Trace

import Common

main = do
  alertsRef <- newIORef M.empty
  initGUI
  timeoutAdd (loop alertsRef >> return True) 1000  -- Repeat every 1s.
  mainGUI

loop alertsRef =
  withReminderFile $ \f -> do
    rs <- readReminders f
    t <- getCurrentTime
    tz <- loadLocalTZ

    -- We show a reminder if it got expired since the moment it was seen, or
    -- if -it got expired since the moment it was last acknowledged- and -was
    -- seen more than 5min ago- (i.e. it'll be shown every 5min if you keep
    -- not acknowledging it).
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
                 ("         Reminder: " ++ message r ++ "         ")

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
