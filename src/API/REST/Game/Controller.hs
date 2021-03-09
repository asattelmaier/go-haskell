{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}



module API.REST.Game.Controller where



import qualified API.REST.Game.Service as Service
import           API.REST.Utils
import           Control.Lens
import qualified Data.ByteString.Char8 as B
import           Snap.Core
import           Snap.Snaplet



data GameController = GameController



makeLenses ''GameController



createGameController :: SnapletInit b GameController
createGameController =
  makeSnaplet "game" "Game Controller" Nothing $ do
  addRoutes gameRoutes
  return GameController



gameRoutes :: [(B.ByteString, Handler b GameController ())]
gameRoutes = [("/", method POST $ handlePOST Service.createGame)]

