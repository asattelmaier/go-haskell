{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Input.Intersection where



import           API.JSON.Input.Location ()
import           API.JSON.Input.State    ()
import           Data.Aeson
import           Go.Board



instance FromJSON Intersection where
  parseJSON (Object v) = Intersection
    <$> v .: "location"
    <*> v .: "state"

