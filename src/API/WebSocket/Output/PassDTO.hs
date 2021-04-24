{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



module API.WebSocket.Output.PassDTO
( DTO (DTO)
) where



import           API.JSON.Output.EndGame
import           API.JSON.Output.Game
import           Data.Aeson



data DTO = DTO { game    :: Maybe Game
               , endGame :: Maybe EndGame
               }



instance ToJSON DTO where
  toJSON (DTO game Nothing) = toJSON game
  toJSON (DTO Nothing game) = toJSON game

