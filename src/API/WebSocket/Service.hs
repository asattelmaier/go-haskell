module API.WebSocket.Service
( create
, play
, pass
) where



import           API.REST.Output.GameDTO
import qualified API.WebSocket.Input.CreateDTO as Create
import qualified API.WebSocket.Input.PassDTO   as Input.Pass
import qualified API.WebSocket.Input.PlayDTO   as Play
import qualified API.WebSocket.Output.PassDTO  as Output.Pass
import           Control.Applicative
import qualified Go.Game                       as Go


create :: Create.DTO -> GameDTO
create = GameDTO . Go.create . Create.getSize



play :: Play.DTO -> GameDTO
play = GameDTO . liftA2 Go.play Play.getGame Play.getLocation



-- TODO: Refactor, try to avoid the flips for the sake of readability
pass :: Input.Pass.DTO -> Output.Pass.DTO
pass = flip maybe (flip Output.Pass.DTO Nothing . Just) . end
       <*> Go.pass . Input.Pass.getGame



end :: Input.Pass.DTO -> Output.Pass.DTO
end = Output.Pass.DTO Nothing . Just . Go.end . Input.Pass.getGame

