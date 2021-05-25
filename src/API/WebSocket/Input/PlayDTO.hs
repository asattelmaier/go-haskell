{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



module API.WebSocket.Input.PlayDTO
( PlayDTO (PlayDTO)
, getGame
, getLocation
, getIsSuicideAllowed
) where



import           API.JSON.Input.Game                (Game)
import           API.JSON.Input.Location            (Location)
import           API.WebSocket.Input.PlayCommandDTO (PlayCommandDTO)
import qualified API.WebSocket.Input.PlayCommandDTO as PlayCommandDTO (getIsSuicideAllowed,
                                                                       getLocation)
import           Data.Aeson                         (FromJSON, Value (Object),
                                                     parseJSON, (.:))



data PlayDTO = PlayDTO { command :: PlayCommandDTO
                       , game    :: Game
                       } 



instance FromJSON PlayDTO where
  parseJSON (Object v) = PlayDTO
    <$> v .: "command"
    <*> v .: "game"



getGame :: PlayDTO -> Game
getGame PlayDTO {..} = game



getCommand :: PlayDTO -> PlayCommandDTO
getCommand = command



getIsSuicideAllowed :: PlayDTO -> Maybe Bool
getIsSuicideAllowed = PlayCommandDTO.getIsSuicideAllowed . getCommand



getLocation :: PlayDTO -> Location
getLocation = PlayCommandDTO.getLocation . getCommand

