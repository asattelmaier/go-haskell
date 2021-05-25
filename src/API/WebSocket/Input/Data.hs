{-# LANGUAGE OverloadedStrings #-}



module API.WebSocket.Input.Data
( Data (CreateDTO, PlayDTO, PassDTO)
) where



import           API.WebSocket.Input.CreateDTO (CreateDTO)
import           API.WebSocket.Input.PassDTO   (PassDTO)
import           API.WebSocket.Input.PlayDTO   (PlayDTO)
import           Control.Applicative           ((<|>))
import           Data.Aeson                    (FromJSON, parseJSON)
import           Data.Aeson.Types              (Parser)



data Data = CreateDTO CreateDTO | PlayDTO PlayDTO | PassDTO PassDTO



instance FromJSON Data where
  parseJSON v =
    (CreateDTO <$> (parseJSON v :: Parser CreateDTO))
    <|> (PlayDTO <$> (parseJSON v :: Parser PlayDTO))
    <|> (PassDTO <$> (parseJSON v :: Parser PassDTO))

