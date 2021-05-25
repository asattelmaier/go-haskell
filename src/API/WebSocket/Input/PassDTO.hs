{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



module API.WebSocket.Input.PassDTO
( PassDTO (PassDTO)
, getGame
) where



import           API.JSON.Input.Game                (Game)
import           API.WebSocket.Input.PassCommandDTO (PassCommandDTO)
import           Data.Aeson                         (FromJSON, Value (Object),
                                                     parseJSON, (.:))



data PassDTO = PassDTO { command :: PassCommandDTO
                       , game    :: Game
                       } deriving (Show)



instance FromJSON PassDTO where
  parseJSON (Object v) = PassDTO
    <$> v .: "command"
    <*> v .: "game"



getGame :: PassDTO -> Game
getGame PassDTO {..} = game

