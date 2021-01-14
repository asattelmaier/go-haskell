module Game
( Game (Game, board, activePlayer, passivePlayer)
, createGame
) where



import Board  (Board, createBoard)
import Player (Player, Color (Black, White), createPlayer)



data Game = Game { board         :: Board
                 , activePlayer  :: Player
                 , passivePlayer :: Player
                 }



createGame :: Int -> Game
createGame lines = Game { board         = createBoard lines lines
                        , activePlayer  = createPlayer Black
                        , passivePlayer = createPlayer White
                        }

