{-# LANGUAGE
RecordWildCards,
TemplateHaskell,
OverloadedStrings,
ViewPatterns,
MultiWayIf,
ScopedTypeVariables,
NoImplicitPrelude
  #-}


module Tom.Daemon
(
  runDaemon,
)
where


-- General
import BasePrelude hiding (on, second)
-- Lenses
import Lens.Micro.Platform hiding ((&), set)
-- Text
import Data.Text (Text)
import qualified Data.Text as T
-- Containers
import Data.Map (Map)
import qualified Data.Map as M
-- GTK
import Graphics.UI.Gtk hiding (currentTime, edited)
-- UUID
import Data.UUID hiding (null)
-- Time
import Data.Time
-- IO
import Control.Monad.IO.Class
-- Tom-specific
import Tom.Reminders
import Tom.When


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
  _buttons  :: [AlertButton],
  _edited   :: Maybe Text }

makeLenses ''AlertState

defaultAlertState :: AlertState
defaultAlertState = AlertState {
  _buttons = [
      AlertButton 5  Minute 1,
      AlertButton 1  Hour   1,
      AlertButton 12 Hour   1 ],
  _edited = Nothing }

type AlertWindow = Dialog

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
    -- automatically removed from varAlertMap because of the “after
    -- objectDestroy” handler we add a bit later (just read on)).
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
    widgetShowAll (alert ^. window)
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

makeAlertWindow
  :: Text                -- ^ Starting text
  -> Bool                -- ^ False = start as label, True = start as editbox
  -> Text                -- ^ Caption
  -> (Text -> IO ())     -- ^ On edit
  -> (Text -> IO ())     -- ^ On finishing edit
  -> IO Dialog
makeAlertWindow startingText startEditable caption onEdit onCommit = do
  dialog <- dialogNew
  set dialog [
    windowResizable := False ]
  windowSetGeometryHints dialog (Nothing :: Maybe Window)
    (Just (300, -1))  -- minWidth, minHeight
    (Just (300, -1))  -- maxWidth, maxHeight
    Nothing
    Nothing
    Nothing
  content <- castToBox <$> dialogGetContentArea dialog

  label <- labelNew (Just (highlightLinks startingText))
  labelBox <- eventBoxNew
  labelBox `containerAdd` label
  set label [
    labelSelectable    := True,
    labelWrap          := True,
    labelMaxWidthChars := 50,
    labelUseMarkup     := True,
    miscXalign         := 0,
    widgetMarginLeft   := 20,
    widgetMarginRight  := 20,
    widgetMarginTop    := 20,
    widgetMarginBottom := 20 ]

  text <- textViewNew
  textFrame <- frameNew
  textFrame `containerAdd` text
  buffer <- get text textViewBuffer
  -- This is done so that onEdit wouldn't be called when ‘text’ is first
  -- populated with startingText.
  set buffer [
    textBufferText := startingText ]
  set text [
    textViewLeftMargin  := 7,
    textViewRightMargin := 7,
    textViewWrapMode    := WrapWord ]
  set textFrame [
    widgetMarginLeft   := 12,  -- 20−7−1 (we want the *text* to remain
    widgetMarginRight  := 12,  -- at the same position) (1 = border width)
    widgetMarginTop    := 20,
    widgetMarginBottom := 20,
    frameShadowType    := ShadowEtchedIn ]

  let labelToText = do
        Rectangle _ _ width _ <- widgetGetAllocation labelBox
        content `containerRemove` labelBox
        content `containerAdd` textFrame
        widgetShowAll textFrame
        -- don't know why −20; should investigate (widths and size requests
        -- and margins are kinda murky)
        widgetSetSizeRequest textFrame (width - 20) (-1)
        textBufferPlaceCursor buffer =<< textBufferGetEndIter buffer
        widgetGrabFocus text
      textToLabel = do
        content `containerRemove` textFrame
        content `containerAdd` labelBox
        widgetShowAll labelBox
        textContents <- get buffer textBufferText
        set label [
          labelText      := highlightLinks textContents,
          labelUseMarkup := True ] -- for some reason this has to be set again
        onCommit textContents
        -- todo: remove selection in text label

  if startEditable
    then content `containerAdd` textFrame
    else content `containerAdd` labelBox

  labelCaption <- labelNew (Just caption)
  set labelCaption [
    labelSelectable    := True,
    miscXalign         := 0,
    widgetMarginLeft   := 20,
    widgetMarginRight  := 20,
    widgetMarginTop    := 0,
    widgetMarginBottom := 20 ]

  boxPackEnd content labelCaption PackNatural 0

  buffer `on` bufferChanged $
    onEdit =<< get buffer textBufferText
  labelBox `on` buttonPressEvent $ tryEvent $ do
    DoubleClick <- eventClick
    LeftButton  <- eventButton
    liftIO $ labelToText
  -- todo: label on Enter should turn into text
  text `on` keyPressEvent $ tryEvent $ do
    "Return" :: Text <- eventKeyName
    [] <- eventModifier  -- so that Shift+Enter would work
    liftIO $ textToLabel

  windowSetGravity  dialog GravityCenter
  windowSetPosition dialog WinPosCenterAlways

  return dialog

createAlert :: UUID -> Reminder -> IORef AlertState -> IO Alert
createAlert uuid reminder varState = do
  let onEdit s = do
        modifyIORef varState $ edited .~ Just s
  let onCommit s = do
        modifyReminder uuid $ message .~ s
        modifyIORef varState $ edited .~ Nothing
  (dialogMessage, startEditable) <- do
    mbE <- view edited <$> readIORef varState
    case mbE of
      Nothing -> return (reminder ^. message, False)
      Just e  -> return (e, True)
  alertWindow <- makeAlertWindow
                   dialogMessage
                   startEditable
                   (T.pack (show (reminder ^. schedule)))  -- caption
                   onEdit
                   onCommit
  -- Add “... later” buttons from state (or default buttons).
  snoozeButtons <- addSnoozeButtons varState alertWindow
  -- Add the rest of the buttons.
  buttonNo  <- dialogAddButton alertWindow ("Turn it off" :: Text) ResponseNo
  buttonYes <- dialogAddButton alertWindow ("Thanks!"     :: Text) ResponseYes
  -- Assign actions to buttons.
  alertWindow `on` response $ \responseId ->
    responseHandler uuid varState alertWindow responseId
  -- Add a handler: when the alert is shown, block the buttons and create a
  -- timer that would unblock them in 1s (to prevent accidental clicks when
  -- the user is doing something and the alert appears suddenly).
  alertWindow `on` showSignal $ void $ do
    let allButtons = buttonNo : buttonYes : snoozeButtons
    for_ allButtons $ \button -> widgetSetSensitive button False
    flip timeoutAdd 1000 $ do
      for_ allButtons $ \button -> widgetSetSensitive button True
      return False  -- Don't repeat the timer.
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
highlightLinks :: Text -> Text
highlightLinks "" = ""
highlightLinks (T.uncons -> Just ('>', s)) =
  "&gt;" <> highlightLinks s
highlightLinks (T.uncons -> Just ('<', s)) = do
  let (link, rest) = T.break (== '>') s
  if -- “<” doesn't have a closing “>”, so it's not a link
     | T.null rest -> "&lt;" <> highlightLinks s
     -- if it's a link, let's convert it
     | isLink link -> do
         let httpLink
               | "://" `T.isInfixOf` link = link
               | otherwise                = "http://" <> link
             markup = T.pack (printf "<a href=\"%s\">%s</a>" httpLink link)
         markup <> highlightLinks (T.tail rest)
     -- otherwise, not a link
     | otherwise -> "&lt;" <> highlightLinks s
highlightLinks s =
  let (text, rest) = T.break (\c -> c == '<' || c == '>') s
  in  text <> highlightLinks rest

{- |
Link-detection algorithm:

  * A link can't be empty.

  * A link has to start with a letter or a digit.

  * There has to be a “.” followed by something that isn't a space and isn't a “.” (to prevent “...” from being perceived as a link).
-}
isLink :: Text -> Bool
isLink s = and [
  not (T.null s),
  isDigit (T.head s) || isLetter (T.head s),
  T.any (== '.') s,
  or $ do ('.', c) <- T.zip s (T.tail s)
          return (not (isSpace c) && c /= '.') ]
