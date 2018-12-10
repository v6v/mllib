module AI.H2048.Minmax(minmaxPlayer) where

import           AI.H2048.Core
import           Data.Function     (on)
import           Data.List         (sortBy, transpose)
import           Data.List.Ordered (isSorted)
import           Data.Maybe        (listToMaybe)


minmaxPlayer :: Player
minmaxPlayer =
    Player (PlayerName "Minmax-AI") nextMove

nextMove :: Board -> IO (Maybe Move)
nextMove b =
         return .
         listToMaybe .
         sortBy (flip compare `on` \m -> scored 6 m b) .
         filter (canMove b)
         $ options
  where
    scored :: Int -> Move -> Board -> Int
    scored 0 _ _  = 0
    scored 1 m b' = score $ move m b'
    scored n m b' = sum . map (\m' -> scored (n-1) m' (move m b')) $ options

score :: Board -> Int
score b = orderScore +
          maxAtEndScore + closePairsScore - divergeScore - emptyScore
  where
    emptyScore = floor $ if en > 10 then 0 else 3 ^ (10-en)
    stat = toStat b
    en = emptyNum stat
    log2Int :: Int -> Int
    log2Int = floor . (2.5^) . floor .  logBase 2.0 . fromIntegral
    orderScore = 6 * sum (map log2Int (ordered stat)) + 100 * length (maxAtEnd stat)
    maxAtEndScore = 4 * sum (map log2Int (maxAtEnd stat)) + 100 * length (maxAtEnd stat)
    closePairsScore = 2 * sum (map log2Int (closePairs stat))
    divergeScore = floor $ if dn < 6 then 0 else 3 ^ (dn - 6)
    dn = length (diverge stat)
