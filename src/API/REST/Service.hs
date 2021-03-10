{-# LANGUAGE RecordWildCards #-}



module API.REST.Service
( createGame
) where



import           API.REST.Input.CreateGameDTO
import           API.REST.Output.GameDTO
import           Control.Lens
import           Data.Maybe
import qualified Go.Game                      as Go



defaultGridSize :: Int
defaultGridSize = 19



createGame :: CreateGameDTO -> GameDTO
createGame = GameDTO . Go.createGame . getSize
  where getSize = fromMaybe defaultGridSize . view size

