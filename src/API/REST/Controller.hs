{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}



module API.REST.Controller where



import           API.REST.Game.Controller
import           Control.Lens
import qualified Data.ByteString.Char8    as B
import           Snap.Core
import           Snap.Snaplet



newtype Controller = Controller { _gameController :: Snaplet GameController
                                }



makeLenses ''Controller



controller :: SnapletInit b Controller
controller =
  makeSnaplet "controller" "Go Haskell REST API Controller" Nothing $ do
  _gameController <- nestSnaplet "game" gameController createGameController
  addRoutes routes
  return $ Controller _gameController



routes :: [(B.ByteString, Handler b Controller ())]
routes = [("status", method GET respondOk)]



respondOk :: Handler b Controller ()
respondOk = do
  modifyResponse . setResponseCode $ 200
