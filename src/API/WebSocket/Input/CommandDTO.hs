{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}



module API.WebSocket.Input.CommandDTO
( CommandDTO (CommandDTO)
, name
, location
, size
) where



import           API.JSON.Input.Command
import           API.JSON.Input.Location
import           Control.Lens
import           Data.Aeson



data CommandDTO = CommandDTO { _name     :: Command
                             , _location :: Maybe Location
                             , _size     :: Maybe Int
                             } deriving (Show)



makeLenses ''CommandDTO



instance FromJSON CommandDTO where
  parseJSON (Object v) = CommandDTO
    <$> v .:  "name"
    <*> v .:? "location"
    <*> v .:? "size"
