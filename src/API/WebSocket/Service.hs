module API.WebSocket.Service
( createGame
, playStone
) where



import           API.REST.Output.GameDTO
import qualified API.WebSocket.Input.CreateGameDTO as CreateGame
import qualified API.WebSocket.Input.PlayStoneDTO  as PlayStone
import qualified Go.Game                           as Go



createGame :: CreateGame.DTO -> GameDTO
createGame = GameDTO . Go.createGame . CreateGame.getSize



playStone :: PlayStone.DTO -> GameDTO
playStone dto = GameDTO . Go.play getGame $ getLocation
  where getGame     = PlayStone.getGame dto
        getLocation = PlayStone.getLocation dto

