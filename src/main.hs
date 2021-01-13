import Data.Char
import Foreign.C.Types
import System.Process
import Command (Command (Exit, Move), createCommand, commandToPosition)
import Render (Grid (Grid), render)
import Cursor (Cursor (Cursor))
import Position (Position (Position))
import Player (Player (Player, cursor, color), Color (Black, White), movePlayer)

-- TODO: Grid should be configurable by the user

main :: IO ()
main = do
  run grid playerBlack playerWhite
  where grid   = Grid 9 9
        playerBlack = Player {cursor = Cursor (Position 0 0), color = Black}
        playerWhite = Player {cursor = Cursor (Position 0 0), color = Black}

run :: Grid -> Player -> Player -> IO ()
run grid activePlayer player = do
--- TODO: This will work only on Windows, find a general solution 
  system "cls"
  putStr $ render grid activePlayer
  userInput <- getHiddenChar

  let command = createCommand userInput
  let position = commandToPosition command

  if (command == Exit)
  then return ()
  else run grid (movePlayer activePlayer position) player

{-# LANGUAGE ForeignFunctionInterface #-}
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
