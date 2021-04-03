{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}



module API.REST.Input.CreateGameDTO
( CreateGameDTO (CreateGameDTO)
, size
) where



import           Control.Lens
import           Data.Aeson



newtype CreateGameDTO = CreateGameDTO { _size :: Maybe Int
                                      } deriving (Show)



makeLenses ''CreateGameDTO



instance FromJSON CreateGameDTO where
  parseJSON (Object v) = CreateGameDTO
    <$> v .:? "size"

