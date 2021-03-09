{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}



module API.REST.Game.Input.CreateGameDTO
( CreateGameDTO (CreateGameDTO, size)
) where



import           Data.Aeson
import           Data.Data



newtype CreateGameDTO = CreateGameDTO { size :: Maybe Int
                                      } deriving (Show, Typeable, Data)



instance FromJSON CreateGameDTO where
  parseJSON (Object v) = CreateGameDTO
    <$> v .:? "size"

