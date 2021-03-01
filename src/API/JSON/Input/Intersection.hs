{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Input.Intersection where



import Data.Aeson
import Go.Board
import API.JSON.Input.Location()
import API.JSON.Input.State()



instance FromJSON Intersection where
  parseJSON (Object v) = Intersection
    <$> v .: "location"
    <*> v .: "state"

