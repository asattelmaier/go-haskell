{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}



module API.WebSocket.Input.Command
( Command (Command)
, Name (CreateGame, PlayStone, Pass)
, name
, location
, size
) where



import           API.JSON.Input.Location
import           Control.Lens
import           Data.Aeson



data Name = CreateGame
          | PlayStone
          | Pass
          deriving (Show)



instance FromJSON Name where
  parseJSON name = case name of
    "CreateGame" -> pure CreateGame
    "PlayStone"  -> pure PlayStone
    "Pass"       -> pure Pass



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
