{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}

module EffectfulServer
  ( run,
  )
where

import Conduit (ConduitT, (.|))
import Conduit qualified as C
import Control.Monad (when)
import Control.Monad.Except (ExceptT (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Effectful (Eff, IOE, UnliftStrategy (..), runEff, withEffToIO, (:>))
import Effectful.Error.Static (Error, runError, throwError)
import Effectful.Reader.Static (Reader, ask, runReader)
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
    ServerError,
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

type App = Eff '[Reader String, Error ServerError, IOE]

runApp :: String -> App a -> Handler a
runApp name m = Handler $ ExceptT $ fmap (first snd) $ runEff $ runError $ runReader name m

type Api =
  "items" S.:> Capture "itemId" Integer S.:> Get '[JSON] Item
    :<|> "stream" S.:> StreamGet NewlineFraming JSON (ConduitT () Item IO ())

server :: ServerT Api App
server =
  getItem
    :<|> stream

getItem :: (Reader String :> es, Error ServerError :> es) => Integer -> Eff es Item
getItem itemId = do
  when (itemId > 5) $ do
    throwError S.err404

  name <- ask
  pure $ Item itemId name

stream :: (IOE :> es, Reader String :> es) => Eff es (ConduitT () Item IO ())
stream =
  withEffToIO SeqForkUnlift $ \runIO -> do
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
