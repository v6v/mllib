module AI.H2048.Random where

import AI.H2048.Core

randomPlayer :: Player
randomPlayer = Player (PlayerName "Random-AI") $
  \b -> pickRandom' $ filter (canMove b) options
