{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}

module MtlServer
  ( run,
  )
where

import Conduit (ConduitT, MonadIO, MonadUnliftIO, (.|))
import Conduit qualified as C
import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp qualified as W
import Servant
  ( Application,
    Capture,
    Get,
    Handler (..),
    HasServer (ServerT),
    JSON,
    NewlineFraming,
    Proxy (..),
    StreamGet,
    (:<|>) (..),
  )
import Servant qualified as S
import Servant.Conduit ()
import System.IO qualified as IO

data Item = Item
  { itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item

instance FromJSON Item

newtype App a = App (ReaderT String IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadReader String,
      MonadThrow
    )

runApp :: String -> App a -> Handler a
runApp name (App m) = Handler $ ExceptT $ try $ runReaderT m name

type Api =
  "items" S.:> Capture "itemId" Integer S.:> Get '[JSON] Item
    :<|> "stream" S.:> StreamGet NewlineFraming JSON (ConduitT () Item IO ())

server :: ServerT Api App
server =
  getItem
    :<|> stream

getItem :: (MonadReader String m, MonadThrow m) => Integer -> m Item
getItem itemId = do
  when (itemId > 5) $ do
    throwM S.err404

  name <- ask
  pure $ Item itemId name

stream :: (MonadUnliftIO m, MonadReader String m) => m (ConduitT () Item IO ())
stream =
  C.withRunInIO $ \runIO -> do
    pure $
      C.yieldMany [1 ..]
        .| C.takeC 10
        .| C.mapMC
          ( \i -> runIO $ do
              name <- ask

              pure $ Item i name
          )

mkApp :: String -> Application
mkApp name =
  S.serve (Proxy @Api) $
    S.hoistServer (Proxy @Api) (runApp name) server

run :: IO ()
run = do
  let port = 3000
  let startMessage = IO.hPutStrLn IO.stderr ("listening on port " ++ show port)
  let settings =
        W.setPort port
          . W.setBeforeMainLoop startMessage
          $ W.defaultSettings
  let app = mkApp "Drew"

  W.runSettings settings app
