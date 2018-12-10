module AI.H2048.Core
  ( PlayerName(..)
  , Player(..)
  , playerName
  , canMove
  , getMove
  , options
  , Move(..)
  , pickRandom'
  , Board(..)
  , emptyCellNum
  , emptyPos
  , move
  , Stat(..)
  , toStat
  ) where

import           H2048

canMove :: Board -> Move -> Bool
canMove b m = move m b /= b

getMove :: Player -> Board -> IO (Maybe Move)
getMove (Player _ strategy) = strategy

options :: [Move]
options = [UP, DOWN, LEFT, RIGHT]

newtype PlayerName = PlayerName String deriving Eq
data Player = Player PlayerName (Board -> IO (Maybe Move))

instance Eq Player where
  (Player n1 _) == (Player n2 _) = n1 == n2

playerName :: Player -> String
playerName (Player (PlayerName n) _) = n

emptyCellNum :: Board -> Int
emptyCellNum Board {_cells = cells} = sum $ map (length . filter (== 0)) cells
