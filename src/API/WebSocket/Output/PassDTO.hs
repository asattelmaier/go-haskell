{-# LANGUAGE RecordWildCards   #-}



module API.WebSocket.Output.PassDTO
( PassDTO (PassDTO)
) where



import           API.JSON.Output.EndGame (EndGame)
import           API.JSON.Output.Game    (Game)
import           Data.Aeson              (ToJSON, toJSON)



data PassDTO = PassDTO { game    :: Maybe Game
                       , endGame :: Maybe EndGame
                       }



instance ToJSON PassDTO where
  toJSON (PassDTO game Nothing) = toJSON game
  toJSON (PassDTO Nothing game) = toJSON game

