{-# LANGUAGE
RecordWildCards,
TemplateHaskell,
NoImplicitPrelude
  #-}


module Tom.Daemon
(
  runDaemon,
)
where


-- General
import BasePrelude hiding (on)
-- Lenses
import           Lens.Micro.Platform hiding ((&))
-- Containers
import qualified Data.Map as M
import           Data.Map (Map)
-- Text
import           Text.Printf
-- GTK
import           Graphics.UI.Gtk hiding (set, currentTime)
import qualified Graphics.UI.Gtk as Gtk
-- UUID
import           Data.UUID hiding (null)
-- Time
import           Data.Time
-- IO
import           Control.Monad.IO.Class
-- Tom-specific
import           Tom.Reminders
import           Tom.When
import           Tom.Utils


data TimeUnit = Minute | Hour

{- |
This defines a snoozing button in an alert window (i.e. any button which isn't “thanks” and “turn it off”).

An example: initially one of the buttons of an alert says “5 minutes”. It has:

@
'_baseRate'   = 1
'_baseUnit'   = 'Minute'
'_multiplier' = 1
@

After right-clicking it twice, it's going to say “15 minutes”, and its multiplier will be increased by 2:

@
'_baseRate'   = 1
'_baseUnit'   = 'Minute'
'_multiplier' = 3
@
-}
data AlertButton = AlertButton {
  baseRate    :: Integer,
  baseUnit    :: TimeUnit,
  _multiplier :: Integer }

-- Note that the only lens that will be created is “multiplier”; other lenses
-- aren't needed and if we create them, GHC will warn us about unused
-- definitions.
makeLenses ''AlertButton

makeButtonLabel :: AlertButton -> String
makeButtonLabel AlertButton{..} =
  printf "%d%s later" (_multiplier*baseRate) (unitAbbr baseUnit)

unitAbbr :: TimeUnit -> String
unitAbbr Minute = "m"
unitAbbr Hour   = "h"

applyButton :: AlertButton -> UTCTime -> UTCTime
applyButton AlertButton{..} time = do
  let n = baseRate*_multiplier
  case baseUnit of
    Minute -> addUTCTime (fromInteger n * 60) time
    Hour   -> addUTCTime (fromInteger n * 3600) time

data AlertState = AlertState {
  _buttons :: [AlertButton] }

makeLenses ''AlertState

defaultAlertState :: AlertState
defaultAlertState = AlertState {
  _buttons = [
      AlertButton 5  Minute 1,
      AlertButton 1  Hour   1,
      AlertButton 12 Hour   1 ] }

type AlertWindow = MessageDialog

data Alert = Alert {
  _window   :: AlertWindow,
  _getState :: IO AlertState }

makeLenses ''Alert

{-
How does all this work:

* There are reminders (in the file).

* For some reminders there are alerts shown (i.e. they already are on the screen). Each alert has a window and some internal state (for instance, button labels) – see 'AlertState'. The user can modify the internal state from the window (e.g. right-click the buttons to make counts on them increase).

* When it's time to show an alert for a reminder, we use 'createAlert'. It's possible that the alert window is already there and just got ignored or hidden by other windows or whatever and we simply need to bring it to front so that the user would see it again; however, we can't do that easily (there's e.g. 'windowPresent', but at least in Gnome instead of -moving the window to the current workspace- it changes the current workspace, and that's annoying), so we have to recreate the window (using the state of the previous window).
-}
runDaemon :: IO ()
runDaemon = do
  varAlerts <- newIORef M.empty
  initGUI
  -- Repeat every 1s.
  timeoutAdd (checkReminders varAlerts >> return True) 1000
  mainGUI

-- | Find all expired reminders, update them, show alerts for them.
checkReminders :: IORef (Map UUID Alert) -> IO ()
checkReminders varAlertMap = withRemindersFile $ \file -> do
  currentTime <- getCurrentTime
  -- We find all expired reminders (and their UUIDs).
  expired <- filterM (isExpired currentTime . snd) (M.assocs (file ^. remindersOn))
  for_ expired $ \(uuid, reminder) -> do
    -- If the old alert is still hanging around, destroy it (and it'll be
    -- automatically removed from varAlertMap).
    mbOldAlert <- M.lookup uuid <$> readIORef varAlertMap
    case mbOldAlert of
      Just alert -> widgetDestroy (alert ^. window)
      Nothing    -> return ()
    -- Create another alert (possibly using state from the old alert).
    newState <- case mbOldAlert of
      Just alert -> alert ^. getState
      Nothing    -> return defaultAlertState
    varState <- newIORef newState
    alert <- createAlert uuid reminder varState
    -- Add a handler: when the alert window is closed, it'll be removed from
    -- the map.
    (alert ^. window) `after` objectDestroy $
      modifyIORef' varAlertMap (M.delete uuid)
    -- Add the alert to the map.
    modifyIORef' varAlertMap (M.insert uuid alert)
    -- Show the alert.
    widgetShow (alert ^. window)
  -- Finally, lastSeen of all snown reminders must be updated.
  let expired' = M.fromList expired & each . lastSeen .~ currentTime
  return $ file & remindersOn %~ M.union expired'

{- |
We want the following behavior:

* When a reminder is shown and ignored, it's shown again in 5min (unless it gets expired again earlier than that – e.g. if it is set to be shown every minute).

* When a reminder is shown and snoozed, it's not shown again until the snooze interval ends, even if it gets expired again in that period.

So, we show a reminder if it's not snoozed and if either holds:

* It got expired since the moment it was seen.

* It got expired since the moment it was last acknowledged, and it was seen more than 5min ago.
-}
isExpired :: UTCTime -> Reminder -> IO Bool
isExpired currentTime reminder = do
  let notSnoozedAnymore = (reminder ^. snoozedUntil) <= currentTime
      seenLongAgo = diffUTCTime currentTime (reminder ^. lastSeen) >= 5*60
  reexpired <- isReminderInInterval
                 (reminder ^. lastSeen, currentTime)
                 (reminder ^. schedule)
  forgotten <- isReminderInInterval
                 (reminder ^. lastAcknowledged, currentTime)
                 (reminder ^. schedule)
  return $ notSnoozedAnymore && (reexpired || (seenLongAgo && forgotten))

createAlert :: UUID -> Reminder -> IORef AlertState -> IO Alert
createAlert uuid reminder varState = do
  let dialogText :: String
      dialogText = printf "Reminder: %s\n\n(%s)"
        (highlightLinks (reminder ^. message))
        (show (reminder ^. schedule))  
  alertWindow <- messageDialogNew
                   Nothing
                   []       -- flags
                   MessageInfo
                   ButtonsNone
                   dialogText
  -- Enable dialog markup (so that links added by 'highlightLinks' would be
  -- rendered as links).
  Gtk.set alertWindow [messageDialogUseMarkup := True]
  -- Add “... later” buttons from state (or default buttons).
  snoozeButtons <- addSnoozeButtons varState alertWindow
  -- Add the rest of the buttons.
  buttonNo  <- dialogAddButton alertWindow "Turn it off" ResponseNo
  buttonYes <- dialogAddButton alertWindow "Thanks!"     ResponseYes
  -- Assign actions to buttons.
  alertWindow `on` response $ \responseId ->
    responseHandler uuid varState alertWindow responseId
  -- Add a handler: when the alert is shown, block the buttons and create a
  -- timer that would unblock them in 1s (to prevent accidental clicks when
  -- the user is doing something and the alert appears suddenly).
  alertWindow `on` showSignal $ do
    let allButtons = buttonNo : buttonYes : snoozeButtons
    for_ allButtons $ \button -> widgetSetSensitive button False
    flip timeoutAdd 1000 $ do
      for_ allButtons $ \button -> widgetSetSensitive button True
      return False  -- Don't repeat the timer.
    return ()
  -- Return created alert.
  return Alert {
    _window   = alertWindow,
    _getState = readIORef varState }

-- | Read buttons from 'AlertState' and add them to the alert window.
addSnoozeButtons :: IORef AlertState -> AlertWindow -> IO [Button]
addSnoozeButtons varState alertWindow = do
  -- First, we have to get the buttons out.
  buttonStates <- view buttons <$> readIORef varState
  -- Okay, let's create the buttons. Just in case:
  --   * buttonWidget = actual button widget in the alert
  --   * buttonDescr  = description of a button
  for (zip [0..] buttonStates) $ \(buttonIndex, buttonState) -> do
    -- Create a button widget (which has the same response code as button's
    -- position in the original list – this lets us distinguish between
    -- buttons).
    buttonWidget <- dialogAddButton alertWindow
                      (makeButtonLabel buttonState)
                      (ResponseUser buttonIndex)
    -- Assign the button an action – on right click, it increases button's
    -- multiplier and updates the label.
    buttonWidget `on` buttonPressEvent $ tryEvent $ do
      -- These 2 lines check that the click is indeed one we need – when the
      -- click is not a right click, or a single click, a pattern match
      -- exception will get thrown and 'tryEvent' will ignore the event.
      SingleClick <- eventClick
      RightButton <- eventButton
      liftIO $ do
        alertState <- readIORef varState
        let newButtonState = over multiplier (+1) $
              alertState ^?! buttons . ix buttonIndex
        buttonSetLabel buttonWidget (makeButtonLabel newButtonState)
        let alertState' = alertState
                            & buttons . ix buttonIndex .~ newButtonState
        writeIORef varState alertState'
    return buttonWidget

responseHandler
  :: UUID -> IORef AlertState -> AlertWindow -> ResponseId -> IO ()
responseHandler uuid varState alertWindow responseId = do
  currentTime <- getCurrentTime
  modifyReminder uuid $ lastSeen .~ currentTime
  case responseId of
    ResponseNo ->
      disableReminder uuid
    ResponseYes -> do
      modifyReminder uuid $
        lastAcknowledged .~ currentTime
      -- If the reminder was a “Moment”, we can turn it off because it'll
      -- never fire again.
      do file <- readRemindersFile
         case file ^? reminderByUUID uuid . schedule of
           Just Moment{} -> disableReminder uuid
           _other        -> return ()
    ResponseUser buttonIndex -> do
      alertState <- readIORef varState
      let buttonState = alertState ^?! buttons . ix buttonIndex
      modifyReminder uuid $
        snoozedUntil .~ applyButton buttonState currentTime
    _other -> return ()
  widgetDestroy alertWindow

-- | Highlight links in reminder text.
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
        let httpLink
              | "://" `isInfixOf` link = link
              | otherwise              = "http://" ++ link
        printf "<a href=\"%s\">%s</a>" httpLink link ++
          highlightLinks (tail rest)
    -- otherwise, not a link
    | otherwise ->
        "&lt;" ++ highlightLinks (link ++ rest)
highlightLinks (c:s) = c : highlightLinks s
