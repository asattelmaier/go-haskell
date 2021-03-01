{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Output.Color where



import Data.Aeson
import Go.Board



instance ToJSON Color where
  toJSON Black = String "Black"
  toJSON White = String "White"

