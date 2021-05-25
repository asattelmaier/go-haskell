{-# LANGUAGE OverloadedStrings #-}



module API.WebSocket.Input.CreateDTO
( CreateDTO (CreateDTO)
, getSettings
) where



import           API.JSON.Input.Settings              (Settings)
import           API.WebSocket.Input.CreateCommandDTO (CreateCommandDTO)
import qualified API.WebSocket.Input.CreateCommandDTO as CreateCommandDTO (getSettings)
import           Data.Aeson                           (FromJSON, Value (Object),
                                                       parseJSON, (.:))



newtype CreateDTO = CreateDTO { command :: CreateCommandDTO
                              }



instance FromJSON CreateDTO where
  parseJSON (Object v) = CreateDTO
    <$> v .: "command"



getSettings :: CreateDTO -> Settings
getSettings = CreateCommandDTO.getSettings . getCommand



getCommand :: CreateDTO -> CreateCommandDTO
getCommand = command

