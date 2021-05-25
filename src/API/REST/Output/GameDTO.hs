{-# LANGUAGE OverloadedStrings #-}



module API.REST.Output.GameDTO
( GameDTO (GameDTO)
) where



import           API.JSON.Output.Game
import           Data.Aeson



newtype GameDTO = GameDTO Game



instance ToJSON GameDTO where
  toJSON (GameDTO game) = toJSON game

