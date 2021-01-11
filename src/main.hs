import Data.Char
import Foreign.C.Types
import UserInput(handleUserInput)
import Render(render, Grid(Grid))
import Cursor(Cursor(Position), updateCursor)

-- TODO: Grid should be configurable by the user

main :: IO ()
main = do
  run grid cursor
  where grid   = Grid 9 9
        cursor = Position 0 0

run grid cursor = do
  putStr $ render grid cursor
  userInput <- getHiddenChar

  if (userInput == '\ESC')
  then return ()
  else run grid $ updateCursor cursor $ handleUserInput userInput 


{-|
 - LANGUAGE ForeignFunctionInterface (Windows Fix)
 - See: https://gitlab.haskell.org/ghc/ghc/-/issues/2189
-}

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
