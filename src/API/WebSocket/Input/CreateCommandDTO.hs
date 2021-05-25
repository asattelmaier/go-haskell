{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



module API.WebSocket.Input.CreateCommandDTO
( CreateCommandDTO (CreateCommandDTO)
, getSettings
) where



import           API.JSON.Input.Settings        (Settings)
import           API.WebSocket.Input.CommandDTO (CommandDTO (Create))
import           Control.Applicative            (empty)
import           Data.Aeson                     (FromJSON, Value (Object),
                                                 parseJSON, (.:))



data CreateCommandDTO = CreateCommandDTO { name     :: CommandDTO
                                         , settings :: Settings
                                         }



instance FromJSON CreateCommandDTO where
  parseJSON (Object v) = do
    name     <- v .: "name"
    settings <- v .: "settings"

    case name of
      Create -> return CreateCommandDTO {..}
      _      -> empty



getSettings :: CreateCommandDTO -> Settings
getSettings = settings

