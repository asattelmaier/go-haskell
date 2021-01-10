import UserInput(handleUserInput)
import Render(render, Grid(Grid))
import Cursor(Cursor(Position), updateCursor)

-- TODO: Grid should be configurable by the user
main = do
  run grid cursor
  where grid   = Grid 9 9
        cursor = Position 0 0


run grid cursor = do
  putStr $ render grid cursor
  userInput:_ <- getLine
  run grid $ updateCursor cursor $ handleUserInput userInput 
