module API.WebSocket.Service
( createGame
, playGame
) where



import           API.REST.Output.GameDTO
import qualified API.WebSocket.Input.CreateGameDTO as CreateGame
import qualified API.WebSocket.Input.PlayGameDTO   as PlayGame
import qualified Go.Game                           as Go



createGame :: CreateGame.DTO -> GameDTO
createGame = GameDTO . Go.createGame . CreateGame.getSize



playGame :: PlayGame.DTO -> GameDTO
playGame dto = GameDTO . Go.play getGame $ getLocation
  where getGame     = PlayGame.getGame dto
        getLocation = PlayGame.getLocation dto

