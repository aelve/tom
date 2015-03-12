import Control.Applicative
import Data.Foldable (for_)
import Data.Time
import Data.Fixed
import Control.Monad
import Data.List
import Graphics.UI.Gtk

import Common

main = do
  initGUI
  timeoutAdd (loop >> return True) 1000
  mainGUI

loop =
  withReminderFile $ \f -> do
    rs <- readReminders f
    t <- getCurrentTime
    let isExpired r = let diff = diffUTCTime t (time r)
                      in  diff >= 0 && diff `mod'` 60 <= 1
    let (expired, rest) = partition isExpired rs
    for_ expired $ \r -> do
      dialog <- messageDialogNew
                  Nothing
                  []       -- flags
                  MessageInfo
                  ButtonsYesNo
                  ("         Reminder: " ++ message r ++ "         ")
      dialog `on` response $ \rid -> do
        when (rid == ResponseYes) $
          removeReminder r
        widgetDestroy dialog
      widgetShow dialog
