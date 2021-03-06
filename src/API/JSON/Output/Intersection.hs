{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Output.Intersection where



import           API.JSON.Output.Location ()
import           API.JSON.Output.State    ()
import           Data.Aeson
import           Go.Board



instance ToJSON Intersection where
  toJSON (Intersection location state) =
    object [ "location" .= location,
             "state"    .= state ]

