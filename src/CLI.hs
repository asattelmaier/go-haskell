module CLI (main) where



import           CLI.Command   (Command (ExitGame, MoveCursor, Pass, PlayStone))
import           CLI.Cursor    (Cursor, createCursor, translateCursor)
import           CLI.Render    (askForGridSize, cursorToLocation, renderEndGame,
                                renderGame)
import           CLI.UserInput (getCommand, getGridSize)
import           Data.Maybe
import           Go.Game       (EndGame (EndGame), Game, create, end, pass,
                                play)



main :: IO ()
main = do
  putStr askForGridSize

  gridSize <- getGridSize

  setup gridSize



setup :: Int -> IO ()
setup gridSize = run game cursor
  where game   = create gridSize
        cursor = createCursor



run :: Game -> Cursor -> IO ()
run game cursor = do
  putStr $ renderGame game cursor

  command <- getCommand

  case command of
    ExitGame               -> return ()

    MoveCursor translation -> run game $ translateCursor cursor translation

    Pass                   -> maybe endGame runGame passGame
      where passGame = pass game
            runGame  = flip run cursor
            endGame  = terminate $ end game

    PlayStone
      | isNothing location -> run game cursor
      | otherwise          -> run playGame cursor
      where playGame = play game $ fromJust location
            location = cursorToLocation cursor



terminate :: EndGame -> IO ()
terminate (EndGame winners score) = do
  putStr $ renderEndGame (winners, score)
  return ()
