{-# LANGUAGE RecordWildCards #-}



module API.REST.Game.Service where



import qualified API.JSON.Output.DTO               as Output
import           API.REST.Game.Input.CreateGameDTO
import           Data.Maybe
import qualified Go.Game                           as Go



defaultGridSize :: Int
defaultGridSize = 19


-- TODO: Create Game DTO
createGame :: CreateGameDTO -> Output.DTO
createGame game = Output.DTO $ Go.createGame $ getSize game
  where getSize CreateGameDTO {..} = fromMaybe 19 size

