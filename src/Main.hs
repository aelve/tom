{-# LANGUAGE
MultiWayIf,
RecordWildCards
  #-}


-- General
import           Control.Applicative
import           Control.Monad
import           Data.Foldable (asum, for_, traverse_, find)
import           Data.Traversable (traverse)
import           Data.Maybe
-- Lenses
import           Lens.Micro.Platform
-- Lists
import           GHC.Exts (sortWith)
import           Data.List (isInfixOf)
-- Containers
import qualified Data.Map as M
import           Data.Map (Map)
-- Text
import           Text.Printf
import           Data.Char
-- Parsing
import           Text.Parsec hiding ((<|>), optional)
import           Text.Parsec.String
-- Time
import           Data.Time
import           Data.Time.Calendar.MonthDay
import           Data.Time.Zones
-- Strictness
import           Control.DeepSeq
import           Control.Exception (evaluate)
-- GTK
import           Graphics.UI.Gtk hiding (set)
import qualified Graphics.UI.Gtk as Gtk
-- UUID
import           Data.UUID hiding (null)
-- IO
import           Data.IORef
import           System.Environment
import           Control.Monad.IO.Class
-- Tom-specific
import           Tom.When
import           Tom.Common


main :: IO ()
main = do
  args <- getArgs
  if | null args || head args == "--sort" -> listReminders args
     | head args == "--daemon"            -> daemonMain
     | otherwise                          -> scheduleReminder args

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
        _ignoreUntil      = time }
  withRemindersFile $ addReminder reminder
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

daemonMain :: IO ()
daemonMain = do
  alertsRef <- newIORef M.empty
  initGUI
  timeoutAdd (loop alertsRef >> return True) 1000  -- Repeat every 1s.
  mainGUI

{-
Note about alerts
============================================================

We can't use 'windowPresent' to show an already existing alert, because at least in Gnome it doesn't work (instead of moving the window to current workspace, it changes the current workspace, and that's annoying). Instead, we save the alert's state (as it has buttons which can be changed by right-clicking them), close the window, and recreate it.
-}

data TimeUnit = Minute | Hour

-- | Get the abbreviation for a time unit.
unitAbbr :: TimeUnit -> String
unitAbbr Minute = "m"
unitAbbr Hour   = "h"

-- | Add @n@ minutes\/hours\/etc to a moment in time.
unitAdd :: Integer -> TimeUnit -> UTCTime -> UTCTime
unitAdd n unit time = case unit of
  Minute -> addUTCTime (fromInteger n * 60) time
  Hour   -> addUTCTime (fromInteger n * 3600) time

data Alert = Alert {
  alertWindow  :: MessageDialog,
  alertButtons :: IORef [(Integer, (Integer, TimeUnit))] }

-- | This is what you can get from an 'Alert' and what you can use to
-- recreate an alert.
data AlertState = AlertState {
  -- | Each button has a base value and a multiplier – for instance, for “5m
  -- later” the stored value would be @(1, (5, Minute))@, and for “15m later”
  -- – @(3, (5, Minute))@.
  alertStateButtons :: [(Integer, (Integer, TimeUnit))] }

getAlertState :: Alert -> IO AlertState
getAlertState Alert{..} = do
  alertStateButtons <- readIORef alertButtons
  return AlertState{..}

createAlert
  :: UUID              -- ^ Reminder ID
  -> Reminder          -- ^ Reminder
  -> Maybe AlertState  -- ^ Saved alert state
  -> IO Alert
createAlert uuid reminder mbState = do
  let dialogText = highlightLinks (reminder ^. message)
  
  alert <- messageDialogNew
             Nothing
             []       -- flags
             MessageInfo
             ButtonsNone
             ("Reminder: " ++ dialogText ++ "\n\n" ++
              "(" ++ show (reminder ^. schedule) ++ ")")

  -- Enable dialog markup (so that links added by 'highlightLinks' would be
  -- rendered as links).
  Gtk.set alert [messageDialogUseMarkup := True]

  -- Add “... later” buttons from state (or default buttons).
  let showLabel (mul, (n, unit)) = show (mul*n) ++ unitAbbr unit ++ " later"
  let buttonDescriptions = case mbState of
        -- When there aren't any buttons yet, we create new ones:
        --   * “5 minutes” with multiplier 1
        --   * “1 hour” with multiplier 1
        --   * “12 hours” with multiplier 1
        Nothing -> zip (repeat 1) [(5, Minute), (1, Hour), (12, Hour)]
        Just st -> alertStateButtons st
  buttonsRef <- newIORef buttonDescriptions
  for_ (zip [0..] buttonDescriptions) $ \(index, buttonDescription) -> do
    button <- dialogAddButton alert
                (showLabel buttonDescription)
                (ResponseUser index)
    -- On right click, the button's multiplier is increased and the label is
    -- updated.
    button `on` buttonPressEvent $ tryEvent $ do
      SingleClick <- eventClick
      RightButton <- eventButton
      liftIO $ do
        buttonState <- (!! index) <$> readIORef buttonsRef
        -- Increase button's multiplier.
        let buttonState' = buttonState & _1 %~ (+1)
        buttonSetLabel button (showLabel buttonState')
        modifyIORef buttonsRef (set (ix index) buttonState')

  -- Add the rest of the buttons.
  dialogAddButton alert "Turn it off" ResponseNo
  dialogAddButton alert "Thanks!"     ResponseYes

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
    time <- getCurrentTime
    withRemindersFile $ do
      -- We don't use the fact that we already have the reminder – it could
      -- change in the file, even tho there's only a second for it to
      -- happen. So, we're only going to act using reminder's UUID.
      let snooze buttonIndex = \file -> do
            (times, (n, unit)) <- (!! buttonIndex) <$> readIORef buttonsRef
            let changeFunc reminder =
                  reminder & lastSeen .~ time
                           & ignoreUntil .~ unitAdd n unit time
            return $ modifyReminder uuid changeFunc file
      let thanks = modifyReminder uuid
            (set lastSeen time . set lastAcknowledged time)
      let seen = modifyReminder uuid
            (set lastSeen time)
      case responseId of
        ResponseUser buttonIndex -> snooze buttonIndex
        ResponseYes              -> return . thanks
        ResponseNo               -> return . disableReminder uuid
        _other                   -> return . seen
    widgetDestroy alert

  return Alert {
    alertWindow  = alert,
    alertButtons = buttonsRef }

loop :: IORef (Map UUID Alert) -> IO ()
loop alertsRef =
  withRemindersFile $ \file -> do
    currentTime <- getCurrentTime
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
          reexpired <- isReminderInInterval
                         (r ^. lastSeen, currentTime)
                         (r ^. schedule)
          forgotten <- isReminderInInterval
                         (r ^. lastAcknowledged, currentTime)
                         (r ^. schedule)
          return $ and [
            currentTime >= (r ^. ignoreUntil),
            or [ reexpired,
                 forgotten && diffUTCTime currentTime (r ^. lastSeen) >= 5*60 ] ]

    expired <- filterM (isExpired . snd) (M.assocs (file ^. remindersOn))
    for_ expired $ \(uuid, reminder) -> do
      putStrLn $ "Reminder " ++ show uuid ++ " has expired."

      -- If the old alert window is still hanging around, close it.
      mbOldAlert <- M.lookup uuid <$> readIORef alertsRef
      mbOldAlertState <- traverse getAlertState mbOldAlert
      traverse_ (widgetDestroy . alertWindow) mbOldAlert
      modifyIORef' alertsRef (M.delete uuid)

      -- Create another alert window.
      alert <- createAlert uuid reminder mbOldAlertState

      -- When the alert window is closed, we remove it from the map.
      alertWindow alert `after` objectDestroy $ do
        modifyIORef' alertsRef (M.delete uuid)

      -- Add the alert to the map.
      modifyIORef' alertsRef (M.insert uuid alert)

      -- Show the alert.
      widgetShow (alertWindow alert)

    -- Finally, lastSeen of all snown reminders must be updated.
    let expired' = M.fromList expired & each . lastSeen .~ currentTime
    return $ file & remindersOn %~ M.union expired'

highlightLinks :: String -> String
highlightLinks "" = ""
highlightLinks ('>':s) = "&gt;" ++ highlightLinks s
highlightLinks ('<':s) = case break (== '>') s of
  -- “<” doesn't have a closing “>”, so it's not a link
  (_, "") -> "&lt;" ++ s
  (link, rest)
    -- empty link isn't a link
    | null link ->
        "&lt;" ++ highlightLinks rest
    -- “<” is followed by <not a letter/digit>, so it's not a link
    | c:_ <- link, not (isLetter c || isDigit c) ->
        "&lt;" ++ highlightLinks (link ++ rest)
    -- link-detection algorithm: if any “.” is followed by something which
    -- isn't a space and isn't “.” (to avoid “...”), it's a link
    | or [not (isSpace c || c == '.') | ('.', c) <- pairs link] -> do
        let httpLink = if "://" `isInfixOf` link
                         then link else "http://" ++ link
        printf "<a href=\"%s\">%s</a>" httpLink link ++
          highlightLinks (tail rest)
    -- otherwise, not a link
    | otherwise ->
        "&lt;" ++ highlightLinks (link ++ rest)
highlightLinks (x:s) = x : highlightLinks s

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs s  = zip s (tail s)
