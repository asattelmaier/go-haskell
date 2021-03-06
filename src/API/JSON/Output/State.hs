{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Output.State where



import           API.JSON.Output.Color ()
import           Data.Aeson
import           Go.Board



instance ToJSON State where
  toJSON Empty         = String "Empty"
  toJSON (Stone color) = toJSON color

