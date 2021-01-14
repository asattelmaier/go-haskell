import Data.Char
import Foreign.C.Types
import System.Process
import Command (Command (Exit, Move, PlaceStone), createCommand, commandToPosition)
import Render (render)
import Board (Board, createBoard)
import Cursor (Cursor (Cursor))
import Position (Position (Position))
import Player (Player (Player), Color (Black, White), movePlayer, createPlayer)

-- TODO: Board should be configurable by the user

main :: IO ()
main = do

  run board playerBlack playerWhite

  where board       = createBoard 9 9
        playerBlack = createPlayer Black
        playerWhite = createPlayer White



run :: Board -> Player -> Player -> IO ()
run board activePlayer player = do
--- TODO: This will work only on Windows, find a general solution 
  system "cls"
  putStr $ render board activePlayer
  userInput <- getHiddenChar

  let command = createCommand userInput
  let position = commandToPosition command
  let movedPlayer = movePlayer activePlayer position
  
  case command of
    Exit -> return ()
    Move _ -> run board movedPlayer player


{-# LANGUAGE ForeignFunctionInterface #-}
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
