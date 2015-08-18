{-# LANGUAGE
RecordWildCards,
TemplateHaskell
  #-}


{- |
How does all this work:

* There are alerts. Each alert has a window and some internal state.

* You can modify the internal state from the window (e.g. right-click the buttons to make counts on them increase).

* When it's time to pop up an alert, its state is saved, then the alert window is destroyed and recreated from the state. (It'd be easier to use 'windowPresent' to show an already existing alert, but we can't do that, because at least in Gnome it doesn't work – instead of moving the window to current workspace, it changes the current workspace, and that's annoying.)
-}
module Tom.GUI.Alert
(
  checkReminders,
)
where


-- General
import           Data.Foldable
import           Data.Traversable
import           Control.Monad
-- Lenses
import           Lens.Micro.Platform
-- Containers
import qualified Data.Map as M
import           Data.Map (Map)
-- Lists
import           Data.List (isInfixOf)
-- Text
import           Text.Printf
import           Data.Char
-- GTK
import           Graphics.UI.Gtk hiding (set)
import qualified Graphics.UI.Gtk as Gtk
-- UUID
import           Data.UUID hiding (null)
-- Time
import           Data.Time
-- IO
import           Data.IORef
import           Control.Monad.IO.Class
-- Tom-specific
import           Tom.Common


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
  _baseRate   :: Integer,
  _baseUnit   :: TimeUnit,
  _multiplier :: Integer }

makeLenses ''AlertButton

makeButtonLabel :: AlertButton -> String
makeButtonLabel AlertButton{..} =
  printf "%d%s later" (_multiplier*_baseRate) (unitAbbr _baseUnit)

unitAbbr :: TimeUnit -> String
unitAbbr Minute = "m"
unitAbbr Hour   = "h"

applyButton :: AlertButton -> UTCTime -> UTCTime
applyButton AlertButton{..} time = do
  let n = _baseRate*_multiplier
  case _baseUnit of
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
      return False  -- Don't repeat the timeout.
    return ()
  -- Return created alert.
  return Alert {
    _window   = alertWindow,
    _getState = readIORef varState }

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

-- | Find all expired reminders, update them, show alerts for them.
checkReminders :: IORef (Map UUID Alert) -> IO ()
checkReminders varAlertMap = withRemindersFile $ \file -> do
  currentTime <- getCurrentTime
  -- We find all expired reminders (and their UUIDs).
  expired <- filterM (isExpired currentTime . snd) (M.assocs (file ^. remindersOn))
  for_ expired $ \(uuid, reminder) -> do
    -- If the old alert is still hanging around, destroy it.
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

-- | Read buttons from 'AlertState' and add them to the alert window.
addSnoozeButtons :: IORef AlertState -> AlertWindow -> IO [Button]
addSnoozeButtons varState alertWindow = do
  -- First, we have to get the buttons out.
  buttonStates <- view buttons <$> readIORef varState
  -- Okay, let's create the buttons. Just in case:
  --   * buttonWidget = actual button widget in the alert
  --   * buttonDescr  = description of a button
  for (zip [0..] buttonStates) $ \(index, buttonState) -> do
    -- Create a button widget (which has the same response code as button's
    -- position in the original list – this lets us distinguish between
    -- buttons).
    buttonWidget <- dialogAddButton alertWindow
                      (makeButtonLabel buttonState)
                      (ResponseUser index)
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
              alertState ^?! buttons . ix index
        buttonSetLabel buttonWidget (makeButtonLabel newButtonState)
        let alertState' = alertState & buttons . ix index .~ newButtonState
        writeIORef varState alertState'
    return buttonWidget

responseHandler
  :: UUID -> IORef AlertState -> AlertWindow -> ResponseId -> IO ()
responseHandler uuid varState alertWindow responseId = do
  currentTime <- getCurrentTime
  modifyReminder uuid $ lastSeen .~ currentTime
  case responseId of
    ResponseNo -> disableReminder uuid
    ResponseYes -> modifyReminder uuid $
                     lastAcknowledged .~ currentTime
    ResponseUser index -> do
      alertState <- readIORef varState
      let buttonState = alertState ^?! buttons . ix index
      modifyReminder uuid $
        snoozedUntil .~ applyButton buttonState currentTime
    other -> return ()
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
        let httpLink = if "://" `isInfixOf` link
                         then link else "http://" ++ link
        printf "<a href=\"%s\">%s</a>" httpLink link ++
          highlightLinks (tail rest)
    -- otherwise, not a link
    | otherwise ->
        "&lt;" ++ highlightLinks (link ++ rest)
highlightLinks (c:s) = c : highlightLinks s

{- |
This function is used in 'highlightLinks'. An example:

>>> pairs "abc"
[('a','b'),('b','c')]
-}
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs s  = zip s (tail s)
