{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Input.Intersection
( Intersection (Intersection)
) where



import           API.JSON.Input.Location ()
import           API.JSON.Input.State    ()
import           Data.Aeson              (FromJSON, Value (Object), parseJSON,
                                          (.:))
import           Go.Board                (Intersection (Intersection))



instance FromJSON Intersection where
  parseJSON (Object v) = Intersection
    <$> v .: "location"
    <*> v .: "state"

