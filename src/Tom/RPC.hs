{-# LANGUAGE
ScopedTypeVariables,
GADTs,
RankNTypes,
OverloadedStrings,
ExistentialQuantification,
NoImplicitPrelude
  #-}


module Tom.RPC
(
  call, call',
  Method(..),
  RPCError(..),
  showError,
  serve,
  MethodHandler,
)
where


-- General
import BasePrelude hiding (yield)
-- Bytestring
import qualified Data.ByteString.Lazy as BSL
-- Containers
import Data.Map (Map)
-- UUID
import Data.UUID
-- Conduit
import Data.Conduit
import Data.Conduit.List as CList
import Data.Conduit.Network as CNetwork
import Data.Conduit.Serialization.Binary as CBinary
-- Binary
import Data.Binary
-- Tom-specific
import Tom.Reminders (Reminder)


data RPCError = ServerNotRunning | NoResponse | ServerError String
  deriving (Show)

showError :: RPCError -> String
showError ServerNotRunning = "couldn't connect to the daemon"
showError NoResponse       = "no response from the daemon"
showError (ServerError s)  = s

data Method a where
  AddReminder :: Reminder -> Method ()
  EnableReminder :: UUID -> Method ()
  DisableReminder :: UUID -> Method ()
  GetRemindersOn :: Method (Map UUID Reminder)
  GetRemindersOff :: Method (Map UUID Reminder)

data SomeMethod = forall a. Binary a => SomeMethod (Method a)

instance Binary SomeMethod where
  put s = case s of
    SomeMethod (AddReminder x)     -> put (0 :: Int) >> put x
    SomeMethod (EnableReminder x)  -> put (1 :: Int) >> put x
    SomeMethod (DisableReminder x) -> put (2 :: Int) >> put x
    SomeMethod GetRemindersOn      -> put (3 :: Int)
    SomeMethod GetRemindersOff     -> put (4 :: Int)
  get = do
    t :: Int <- get
    case t of
      0 -> SomeMethod . AddReminder     <$> get
      1 -> SomeMethod . EnableReminder  <$> get
      2 -> SomeMethod . DisableReminder <$> get
      3 -> pure (SomeMethod GetRemindersOn)
      4 -> pure (SomeMethod GetRemindersOff)
      n -> error $ "SomeMethod: unknown tag value: " ++ show n

type MethodHandler = forall a. Binary a => Method a -> IO (Either String a)

serve :: MethodHandler -> IO ()
serve handler = do
  let settings = serverSettings 54160 "*"
  runTCPServer settings $ \appData -> do
    appSource appData $$
      CBinary.conduitDecode =$=
      CList.mapM (\(SomeMethod x) -> BSL.toStrict . encode <$> handler x) =$=
      appSink appData

-- TODO: test several ports (random but predetermined) instead of using a
-- predefined value
call :: Binary a => Method a -> IO (Either RPCError a)
call x = do
  let encoded = BSL.toStrict (encode (SomeMethod x))
      settings = clientSettings 54160 "127.0.0.1"
  mbRes <- try $ runTCPClient settings $ \appData -> do
    yield encoded $$ appSink appData
    mbRes <- appSource appData $$ CBinary.conduitDecode =$= await
    return $ case mbRes of
      Nothing          -> Left NoResponse
      Just (Left err)  -> Left (ServerError err)
      Just (Right res) -> Right res
  case mbRes of
    Left e
      | isDoesNotExistError e -> return (Left ServerNotRunning)
      | otherwise             -> ioError e
    Right res                 -> return res  -- can still be Left
                                             -- (for instance, Left NoResponse)

call' :: Binary a => Method a -> IO a
call' x = do
  mbRes <- call x
  case mbRes of
    Left e -> error (showError e)
    Right res -> return res
