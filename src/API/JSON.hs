{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



module API.JSON
( main
) where



import           API.JSON.Input.Command
import qualified API.JSON.Input.DTO         as Input
import qualified API.JSON.Output.DTO        as Output
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe
import           Go.Board
import           Go.Game



defaultGridSize :: Int
defaultGridSize = 19



main :: String -> IO ()
main jsonString = do
  let inputDTO = decode (BL.pack jsonString) :: Maybe Input.DTO

  maybe (return ()) handleInputDTO inputDTO



handleInputDTO :: Input.DTO -> IO ()
handleInputDTO Input.DTO {..} =


  -- TODO: This logic can be reused.
  case command of
    NewGame   -> response $ Output.DTO newGame
      where newGame = createGame $ fromMaybe defaultGridSize size


    Pass      -> maybe (responseScore endGame) responseGame passGame
      where responseGame updatedGame = response $ Output.DTO updatedGame
            passGame     = pass (fromJust game)
            endGame      = end (fromJust game)


    PlayStone -> response $ Output.DTO playGame
      where playGame = play (fromJust game) (fromJust location)



response :: Output.DTO -> IO ()
response outputDTO = BL.putStrLn $ encode outputDTO



-- TODO: Add implementation for response Score
responseScore :: ([Color], Score) -> IO ()
responseScore score = BL.putStrLn (encode score)

