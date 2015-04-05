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
                 ButtonsNone
                 ("Reminder: " ++ message reminder ++ "\n\n" ++
                  "(" ++ show (mask reminder) ++ ")")
      dialogAddButton alert ("5min later"  :: String) (ResponseUser (5*60))
      dialogAddButton alert ("1h later"    :: String) (ResponseUser 3600)
      dialogAddButton alert ("Tomorrow"    :: String) (ResponseUser 86400)
      dialogAddButton alert ("Turn it off" :: String)  ResponseNo
      dialogAddButton alert ("Thanks!"     :: String)  ResponseYes

      -- Processing a response goes as follows:
      -- 
      -- + lastSeen is updated.
      -- 
      -- + On “snooze” ignoreUntil is updated.
      -- + On “turn it off” the reminder is moved into another file.
      -- + On “thanks” lastAcknowledged is updated.
      -- + Closing the reminder does nothing extra.
      -- 
      -- + The alert window is closed.
      alert `on` response $ \responseId -> do
        t <- getCurrentTime
        withRemindersFile . fmap return $ do
          -- We don't use the fact that we already have the reminder (r). It
          -- could change in the file, even tho there's only a second for it
          -- to happen. So, we're only going to act using reminder's UUID.
          let snooze s = modifyReminder uuid $ \reminder ->
                reminder { lastSeen    = t
                         , ignoreUntil = addUTCTime s t }
          let thanks   = modifyReminder uuid $ \reminder ->
                reminder { lastSeen         = t
                         , lastAcknowledged = t }
          let seen     = modifyReminder uuid $ \reminder ->
                reminder { lastSeen = t }
          case responseId of
            ResponseUser s -> snooze (fromIntegral s)
            ResponseYes    -> thanks
            ResponseNo     -> disableReminder uuid
            _other         -> seen
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
