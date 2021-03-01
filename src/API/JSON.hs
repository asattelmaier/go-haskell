{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}



module API.JSON
( main
) where



import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified API.JSON.Input.DTO as Input
import qualified API.JSON.Output.DTO as Output
import API.JSON.Input.Command
import Go.Board
import Go.Game



main :: String -> IO ()
main jsonString = do
  let inputDTO = decode (BL.pack jsonString) :: Maybe Input.DTO

  maybe (return ()) handleInputDTO inputDTO



handleInputDTO :: Input.DTO -> IO ()
handleInputDTO Input.DTO {..} =

  
  case command of
    NewGame   -> response $ Just $ Output.DTO newGame command location size
      where newGame = Just $ createGame $ fromMaybe 19 size

   
    Pass      -> maybe (responseScore endGame) responseGame passGame
      where responseGame updatedGame = response $ Just $ Output.DTO updatedGame command location size
            passGame     = Just $ pass (fromJust game)
            endGame      = end (fromJust game)
    

    PlayStone -> response $ Just $ Output.DTO playGame command location size
      where playGame = Just $ play (fromJust game) (fromJust location)



response :: Maybe Output.DTO -> IO ()
response playData = BL.putStrLn (maybe BL.empty encode playData)



-- TODO: Add implementation for response Score
responseScore :: ([Color], Score) -> IO ()
responseScore score = BL.putStrLn (encode score)
