{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}



module API.WebSocket.Input.PlayStoneDTO
( DTO (DTO)
, getGame
, getLocation
, fromData
) where



import           API.JSON.Input.Game
import           API.JSON.Input.Location
import qualified API.WebSocket.Input.Data       as Data
import           Control.Lens



data DTO = DTO { _game     :: Game
               , _location :: Location
               } deriving (Show)



makeLenses ''DTO



fromData :: Data.Data -> Maybe DTO
fromData inputData =
  case (Data.getGame inputData, Data.getCommandLocation inputData) of
    (Nothing, _)                 -> Nothing
    (_, Nothing)                 -> Nothing
    (Just _game, Just _location) -> Just $ DTO _game _location



getGame :: DTO -> Game
getGame = view game



getLocation :: DTO -> Location
getLocation = view location

