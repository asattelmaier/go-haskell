module UserInterface.UserInput
( getUserInput
) where



import System.IO



getUserInput :: IO Char
getUserInput = do
  hSetBuffering stdin NoBuffering 
  clearLine
  getChar
  


clearLine :: IO ()
clearLine = putStr "\ESC[2K\ESC[0G"
