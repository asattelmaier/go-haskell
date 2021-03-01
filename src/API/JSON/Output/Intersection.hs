{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Output.Intersection where



import Data.Aeson
import Go.Board
import API.JSON.Output.Location()
import API.JSON.Output.State()



instance ToJSON Intersection where
  toJSON (Intersection location state) =
    object [ "location" .= location,
             "state"    .= state ]

