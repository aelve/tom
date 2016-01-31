{-# LANGUAGE
ViewPatterns,
OverloadedStrings,
NoImplicitPrelude
  #-}


module Main
(
  main,
  testMainWindow,
)
where


-- General
import BasePrelude hiding (try, on)
-- IO
import Control.Monad.IO.Class
-- Lenses
import Lens.Micro.Platform hiding (set)
-- Containers
import qualified Data.Map as M
-- Text
import Data.Text (Text)
import qualified Data.Text as T
-- Parsing
import Text.Megaparsec
-- Time
import Data.Time
-- GUI
import Graphics.UI.Gtk
-- Tom-specific
import Tom.When
import Tom.Reminders
import Tom.Utils
import qualified Tom.RPC as RPC


main :: IO ()
main = do
  args <- getArgs
  case args of
    []                 -> runGUI
    ["--list"]         -> listReminders "created"
    ["--list", method] -> listReminders method
    _                  ->
      let (sch, T.drop 2 -> msg) = T.breakOn "::" (T.pack (unwords args))
      in  scheduleReminder (T.strip sch) (T.strip msg)

scheduleReminder :: Text -> Text -> IO ()
scheduleReminder scheduleStr msg = do
  time <- getCurrentTime
  let scheduleP = choice $ map (\p -> try (p <* eof)) allWhenParsers
  _schedule <- case parse scheduleP "" scheduleStr of
    Left err -> error (show err)
    Right x  -> x
  RPC.call' $ RPC.AddReminder $ Reminder {
    _schedule         = _schedule,
    _message          = msg,
    _created          = time,
    _lastSeen         = time,
    _lastAcknowledged = time,
    _snoozedUntil     = time,
    _secret           = False }

listReminders :: String -> IO ()
listReminders method = do
  rsOn  <- RPC.call' RPC.GetRemindersOn
  rsOff <- RPC.call' RPC.GetRemindersOff
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

-- todo: gray placeholders like “reminder text” and “when should it run”

runGUI :: IO ()
runGUI = do
  -- Initialise GTK.
  initGUI
  -- Create our GUI.
  (window, scheduleEntry, scheduleInfo, reminderEntry, secretCheckBox) <-
    createGUI
  -- Close the program when the window is closed.
  window `on` objectDestroy $ mainQuit
  -- When Enter is pressed in the schedule entry box, move focus to the
  -- reminder entry box.
  scheduleEntry `on` entryActivated $
    widgetGrabFocus reminderEntry
  --
  scheduleEntry `on` editableChanged $ do
    t <- get scheduleEntry entryText
    if T.null t
      then set scheduleInfo [labelText := ("enter the schedule" :: Text)]
      else case parseSchedule t of
             Left err -> set scheduleInfo [labelText := err]
             Right _  -> set scheduleInfo [labelText := ("schedule is valid" :: Text)]
  -- When Enter is pressed in the reminder entry box, schedule the reminder.
  reminderEntry `on` keyPressEvent $ tryEvent $ do
    "Return" <- T.unpack <$> eventKeyName
    [] <- eventModifier  -- so that Shift+Enter would work
    scheduleText <- liftIO $ get scheduleEntry entryText
    case parseSchedule scheduleText of
      Left _ -> return ()
      Right getSchedule -> liftIO $ do
        _schedule <- getSchedule
        _message <- do
          buffer <- get reminderEntry textViewBuffer
          get buffer textBufferText
        time <- getCurrentTime
        _secret <- get secretCheckBox toggleButtonActive
        res <- RPC.call $ RPC.AddReminder $ Reminder {
          _schedule         = _schedule,
          _message          = _message,
          _created          = time,
          _lastSeen         = time,
          _lastAcknowledged = time,
          _snoozedUntil     = time,
          _secret           = _secret }
        case res of
          Right _ -> widgetDestroy window
          Left err -> do
            d <- messageDialogNew
              (Just window)
              [DialogModal]
              MessageError
              ButtonsOk
              ("Error: " ++ RPC.showError err)
            dialogRun d
            widgetDestroy d
  -- Show the window and start GTK event loop.
  widgetShowAll window
  mainGUI

createGUI :: IO (Window, Entry, Label, TextView, CheckButton)
createGUI = do
  -- Create main window.
  window <- windowNew
  -- Set window's attributes:
  --   * amount of space around widgets contained in the window = 10
  --   * window must be centered
  --   * window's default width = 400
  set window [
    windowTitle := ("Tom" :: Text),
    containerBorderWidth := 10,
    windowGravity := GravityCenter,
    windowWindowPosition := WinPosCenter,
    windowDefaultWidth := 400 ]
  -- Create a text box for entering schedule (“11pm” or something).
  scheduleEntry <- entryNew
  -- Create a label for showing information about entered schedule.
  scheduleInfo <- labelNew (Just ("enter the schedule" :: Text))
  set scheduleInfo [
    miscXalign := 0,
    miscXpad := 1,
    labelEllipsize := EllipsizeEnd ]
  -- Create a text box for entering the reminder itself.
  reminderEntry <- textViewNew
  -- Text box's attributes:
  --   * there are left and right margins (otherwise it looks ugly)
  --   * text is wrapped by words, but if a word is too long, it will cause
  --     a scrollbar to appear (instead of getting broken in halves)
  --   * Tab isn't accepted as a character (so that the user could use it
  --     to switch back to scheduleEntry)
  set reminderEntry [
    textViewLeftMargin := 7,
    textViewRightMargin := 7,
    textViewWrapMode := WrapWord,
    textViewAcceptsTab := False ]
  -- Add top/bottom margins to reminderEntry. They have a *slightly*
  -- different color than the rest of the textbox, but I don't know of a
  -- better way to do this (e.g. adding padding with CSS doesn't work).
  widgetModifyBase reminderEntry StateActive (Color 30 30 30)
  textViewSetBorderWindowSize reminderEntry TextWindowTop 4
  textViewSetBorderWindowSize reminderEntry TextWindowBottom 4
  -- Create a ScrolledWindow containing reminderEntry, so that it would have
  -- scrollbars when there's too much text in it.
  reminderEntryScrolled <- scrolledWindowNew Nothing Nothing
  reminderEntryScrolled `containerAdd` reminderEntry
  -- Set attributes:
  --   * a nice border around reminderEntry
  --   * minimum size = 200×150
  set reminderEntryScrolled [
    scrolledWindowShadowType := ShadowEtchedIn,
    widgetWidthRequest := 200,
    widgetHeightRequest := 150 ]
  -- Create a “secret reminder” checkbox.
  secretCheckBox <- checkButtonNewWithLabel
    ("Secret reminder (hides reminder text until you click on it)" :: Text)
  -- Create a layout (simply a table where widgets would be placed), put all
  -- other widgets there, and make the table a child of the main window.
  layout <- tableNew 4 1 False  -- 4 rows, 1 column, autostretching = off
  tableAttachDefaults layout scheduleEntry 0 1 0 1
  tableAttachDefaults layout scheduleInfo 0 1 1 2
  tableAttachDefaults layout reminderEntryScrolled 0 1 2 3
  tableAttachDefaults layout secretCheckBox 0 1 3 4
  window `containerAdd` layout
  -- Set layout's attributes:
  --   * amount of space between widgets = 5
  --   * only reminder entry can take vertical space when the window is
  --     expanded
  set layout [
    tableRowSpacing := 5,
    tableChildYOptions scheduleEntry := [Fill],
    tableChildYOptions scheduleInfo := [Fill],
    tableChildYOptions secretCheckBox := [Fill] ]
  return (window, scheduleEntry, scheduleInfo, reminderEntry, secretCheckBox)

parseSchedule :: Text -> Either String (IO When)
parseSchedule s = either (Left . show) Right $ parse scheduleP "" s
  where
    scheduleP = choice $ map (\p -> try (p <* eof)) allWhenParsers

testMainWindow :: IO ()
testMainWindow = testGUI (view _1 <$> createGUI)
