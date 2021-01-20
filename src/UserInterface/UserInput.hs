module UserInterface.UserInput
( getUserInput
) where



import System.IO



getUserInput :: IO Char
getUserInput = do
  hSetBuffering stdin NoBuffering
  getChar
