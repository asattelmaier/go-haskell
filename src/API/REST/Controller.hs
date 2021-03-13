{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}



module API.REST.Controller
( create
) where



import qualified API.REST.Service      as Service
import           API.REST.Utils
import qualified Data.ByteString.Char8 as B
import           Snap.Core
import           Snap.Snaplet



data Controller = Controller



create :: SnapletInit b Controller
create =
  makeSnaplet "game" "Game Controller" Nothing $ do
  addRoutes routes
  return Controller



routes :: [(B.ByteString, Handler b Controller ())]
routes = [("/game", createGame)]



createGame :: Handler b Controller ()
createGame = method POST $ handlePOST Service.createGame

