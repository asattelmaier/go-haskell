{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}



module API.JSON.Output.DTO
( DTO (DTO, game, command, location, size)
) where



import Data.Aeson
import API.JSON.Input.Command
import API.JSON.Output.Game
import API.JSON.Output.Location



-- TODO: Adjust API, only the game is relevant for output
data DTO = DTO { game     :: Maybe Game
               , command  :: Command
               , location :: Maybe Location
               , size     :: Maybe Int
               } deriving (Show)



instance ToJSON DTO where
  toJSON (DTO game command location size) =
    object [ "game"     .= game,
             "command"  .= command,
             "location" .= location ]

