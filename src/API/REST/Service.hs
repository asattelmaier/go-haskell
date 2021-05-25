module API.REST.Service
( createGame
) where



import           API.REST.Input.CreateGameDTO (CreateGameDTO, size)
import           API.REST.Output.GameDTO      (GameDTO (GameDTO))
import           Control.Lens                 (view)
import qualified Go.Game                      as Game (create)
import           Go.Settings                  (Settings (Settings))



createGame :: CreateGameDTO -> GameDTO
createGame dto = GameDTO . Game.create $ settings
  where settings = Settings (view size dto) Nothing

