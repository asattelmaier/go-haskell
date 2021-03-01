{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Output.State where



import Data.Aeson
import Go.Board
import API.JSON.Output.Color()



instance ToJSON State where
  toJSON Empty         = String "Empty"
  toJSON (Stone color) = toJSON color

