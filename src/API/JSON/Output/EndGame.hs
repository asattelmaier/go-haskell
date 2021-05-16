{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Output.EndGame
( EndGame
) where



import           API.JSON.Output.Color ()
import           Data.Aeson
import           Go.Game



instance ToJSON EndGame where
  toJSON (EndGame winner score) =
    object [ "winner" .= winner,
             "score"  .= score ]

