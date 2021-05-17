module Go.Player
( Player
, createBlackPlayer
, createWhitePlayer
) where



import           Go.Color (Color (Black, White))



type Player = Color



createBlackPlayer :: Player
createBlackPlayer = Black



createWhitePlayer :: Player
createWhitePlayer = White

