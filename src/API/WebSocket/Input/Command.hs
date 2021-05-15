{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}



module API.WebSocket.Input.Command
( Command (Command)
, Name (Create, Play, Pass)
, name
, location
, size
) where



import           API.JSON.Input.Location
import           Control.Lens
import           Data.Aeson



data Name = Create
          | Play
          | Pass
          deriving (Show)



instance FromJSON Name where
  parseJSON name = case name of
    "Create" -> pure Create
    "Play"   -> pure Play
    "Pass"   -> pure Pass



data Command = Command { _name     :: Name
                       , _location :: Maybe Location
                       , _size     :: Maybe Int
                       } deriving (Show)



makeLenses ''Command



instance FromJSON Command where
  parseJSON (Object v) = Command
    <$> v .:  "name"
    <*> v .:? "location"
    <*> v .:? "size"
