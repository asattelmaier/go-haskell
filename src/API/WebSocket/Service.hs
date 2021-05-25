module API.WebSocket.Service
( create
, play
, pass
) where



import           API.JSON.Output.Game          (Game)
import           API.WebSocket.Input.CreateDTO (CreateDTO)
import qualified API.WebSocket.Input.CreateDTO as CreateDTO (getSettings)
import qualified API.WebSocket.Input.PassDTO   as Input (PassDTO)
import qualified API.WebSocket.Input.PassDTO   as Input.PassDTO (getGame)
import           API.WebSocket.Input.PlayDTO   (PlayDTO)
import qualified API.WebSocket.Input.PlayDTO   as PlayDTO (getGame, getLocation)
import qualified API.WebSocket.Output.PassDTO  as Output (PassDTO (PassDTO))
import           Control.Applicative           (liftA2)
import qualified Go.Game                       as Game (create, end, pass, play)



create :: CreateDTO -> Game
create = Game.create . CreateDTO.getSettings



play :: PlayDTO -> Game
play = liftA2 Game.play PlayDTO.getGame PlayDTO.getLocation



-- TODO: Refactor, try to avoid the flips for the sake of readability
pass :: Input.PassDTO -> Output.PassDTO
pass = flip maybe (flip Output.PassDTO Nothing . Just) . end
       <*> Game.pass . Input.PassDTO.getGame



end :: Input.PassDTO -> Output.PassDTO
end = Output.PassDTO Nothing . Just . Game.end . Input.PassDTO.getGame

