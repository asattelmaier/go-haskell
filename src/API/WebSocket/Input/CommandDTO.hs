{-# LANGUAGE OverloadedStrings #-}



module API.WebSocket.Input.CommandDTO
( CommandDTO (Create, Play, Pass)
) where



import           Data.Aeson (FromJSON, parseJSON)



data CommandDTO = Create
                | Play
                | Pass
                deriving (Show)



instance FromJSON CommandDTO where
  parseJSON name = case name of
    "Create" -> pure Create
    "Play"   -> pure Play
    "Pass"   -> pure Pass

