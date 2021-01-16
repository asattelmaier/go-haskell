module Main where



import Data.Char
import Data.Maybe
import System.Process
import System.IO
import Command             (Command (ExitGame, MoveCursor, PlaceStone), createCommand)
import Game                (Game (Game, board, activePlayer, passivePlayer), createGame, placeStone)
import Render              (render, cursorToPosition)
import Cursor              (Cursor, createCursor, translateCursor)



main :: IO ()
main = do

  run game cursor

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
    ExitGame          -> return ()
    MoveCursor vector -> run game (translateCursor cursor vector)
    PlaceStone
      | isNothing position -> run game cursor
      | otherwise          -> run (placeStone game (fromJust position)) cursor
      where position = cursorToPosition cursor




getUserInput :: IO Char
getUserInput = do
  hSetBuffering stdin NoBuffering
  getChar

