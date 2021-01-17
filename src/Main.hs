module Main where



import Data.Char
import Data.Maybe
import System.Process
import System.IO
import Game                  (Game (Game, board, activePlayer, passivePlayer), createGame, play)
import UserInterface.Command (Command (ExitGame, MoveCursor, PlaceStone), createCommand)
import UserInterface.Cursor  (Cursor, createCursor, translateCursor)
import UserInterface.Render  (render, cursorToLocation)



main :: IO ()
main = run game cursor

-- TODO: Board should be configurable by the user
  where game   = createGame 9
        cursor = createCursor



run :: Game -> Cursor -> IO ()
run game cursor = do
  system "clear"

  putStr $ render game cursor
  userInput <- getUserInput

  let command = createCommand userInput
  
  case command of
    ExitGame               -> return ()
    MoveCursor translation -> run game (translateCursor cursor translation)
    PlaceStone
      | isNothing location -> run game cursor
      | otherwise          -> run (play game (fromJust location)) cursor
      where location = cursorToLocation cursor




getUserInput :: IO Char
getUserInput = do
  hSetBuffering stdin NoBuffering
  getChar

