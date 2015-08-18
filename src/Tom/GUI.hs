module Tom.GUI
(
  runGUI,
)
where


-- Containers
import qualified Data.Map as M
-- IORef
import Data.IORef
-- GTK
import Graphics.UI.Gtk
-- Tom-specific
import Tom.GUI.Alert
  

runGUI :: IO ()
runGUI = do
  varAlerts <- newIORef M.empty
  initGUI
  -- Repeat every 1s.
  timeoutAdd (checkReminders varAlerts >> return True) 1000
  mainGUI
