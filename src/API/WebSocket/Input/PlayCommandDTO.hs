{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



module API.WebSocket.Input.PlayCommandDTO
( PlayCommandDTO (PlayCommandDTO)
, getLocation
, getIsSuicideAllowed
) where



import           API.JSON.Input.Location        (Location)
import           API.WebSocket.Input.CommandDTO (CommandDTO (Play))
import           Control.Applicative            (empty)
import           Data.Aeson                     (FromJSON, Value (Object),
                                                 parseJSON, (.:), (.:?))



data PlayCommandDTO = PlayCommandDTO { name             :: CommandDTO
                                     , location         :: Location
                                     , isSuicideAllowed :: Maybe Bool
                                     } deriving (Show)



instance FromJSON PlayCommandDTO where
  parseJSON (Object v) = do
    name             <- v .:  "name"
    location         <- v .:  "location"
    isSuicideAllowed <- v .:? "isSuicideAllowed"

    case name of
      Play -> return PlayCommandDTO {..}
      _    -> empty



getLocation :: PlayCommandDTO -> Location
getLocation = location



getIsSuicideAllowed :: PlayCommandDTO -> Maybe Bool
getIsSuicideAllowed = isSuicideAllowed

