{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Input.DTO
( DTO (DTO, game, command, location, size)
) where



import Data.Aeson
import API.JSON.Input.Game
import API.JSON.Input.Location
import API.JSON.Input.Command



data DTO = DTO { game     :: Maybe Game
               , command  :: Command
               , location :: Maybe Location
               , size     :: Maybe Int
               } deriving (Show)



instance FromJSON DTO where
  parseJSON (Object v) = DTO
    <$> v .:? "game"
    <*> v .:  "command"
    <*> v .:? "location"
    <*> v .:? "size"

