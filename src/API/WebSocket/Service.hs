module API.WebSocket.Service
( createGame
) where



import           API.REST.Input.CreateGameDTO
import           API.REST.Output.GameDTO
import           Control.Lens
import           Data.Maybe                   (fromMaybe)
import qualified Go.Game                      as Go



defaultGridSize :: Int
defaultGridSize = 19



createGame :: Maybe CreateGameDTO -> GameDTO
createGame = GameDTO . Go.createGame . getSize . fromMaybe emptyDTO
  where emptyDTO = CreateGameDTO Nothing
        getSize = fromMaybe defaultGridSize . view size

