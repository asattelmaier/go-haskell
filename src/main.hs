import Data.Char
import Foreign.C.Types
import System.Process
import Command (Command (Exit, Move), createCommand, commandToPosition)
import Render (Grid (Grid), render)
import Cursor (Cursor (Cursor), updateCursor)
import Position (Position (Position))

-- TODO: Grid should be configurable by the user

main :: IO ()
main = do
  run grid cursor
  where grid   = Grid 9 9
        cursor = Cursor (Position 0 0)

run grid cursor = do
--- TODO: This will work only on Windows, find a general solution 
  system "cls"
  putStr $ render grid cursor
  userInput <- getHiddenChar

  let command = createCommand userInput
  let position = commandToPosition command

  if (command == Exit)
  then return ()
  else run grid $ updateCursor cursor position

{-# LANGUAGE ForeignFunctionInterface #-}
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
