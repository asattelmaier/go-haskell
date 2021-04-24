module API.WebSocket.Service
( createGame
, playStone
, pass
) where



import           API.REST.Output.GameDTO
import qualified API.WebSocket.Input.CreateGameDTO as CreateGame
import qualified API.WebSocket.Input.PassDTO       as Input.Pass
import qualified API.WebSocket.Input.PlayStoneDTO  as PlayStone
import qualified API.WebSocket.Output.PassDTO      as Output.Pass
import           Control.Applicative
import qualified Go.Game                           as Go


createGame :: CreateGame.DTO -> GameDTO
createGame = GameDTO . Go.createGame . CreateGame.getSize



playStone :: PlayStone.DTO -> GameDTO
playStone = GameDTO . liftA2 Go.play PlayStone.getGame PlayStone.getLocation



-- TODO: Refactor, try to avoid the flips for the sake of readability
pass :: Input.Pass.DTO -> Output.Pass.DTO
pass = flip maybe (flip Output.Pass.DTO Nothing . Just) . end
       <*> Go.pass . Input.Pass.getGame



end :: Input.Pass.DTO -> Output.Pass.DTO
end = Output.Pass.DTO Nothing . Just . Go.end . Input.Pass.getGame

