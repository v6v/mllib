module AI.H2048.Expt(alphaBetaPlayer,
                     exptPlayer
                    ) where

import           AI.H2048.Core
import           Control.Applicative
import           Control.Arrow       (second)
import           Control.Lens
import           Control.Monad       (join)
import           Data.Function       (on)
import           Data.List           (maximum, maximumBy, sortBy, transpose)
import           Data.List.Ordered   (isSorted)
import           Data.Maybe          (listToMaybe, maybeToList, fromMaybe)
import           Data.Ord            (comparing)

data ProbBoard = PM [(Move,ProbBoard)] | PP [(Double,ProbBoard)] | PL Board
  deriving Show

toPB :: Board -> ProbBoard
toPB b = PP $ next2s ++ next4s
  where pos = emptyPos b
        l :: Double
        l = fromInteger . toInteger $ length pos
        next2s = pvboards 0.9 2
        next4s = pvboards 0.1 4
        pvboards p v = map ((\b' -> (p/l, PL b')) . nextBoards v b) pos

nextBoards :: Int -> Board -> (Int,Int) ->  Board
nextBoards v b (x,y) = b {_cells = _cells b & ix x . ix y .~ v}

step :: ProbBoard -> ProbBoard
step (PL b) = PM . map (liftA2 (,) id (toPB . flip move b)) . filter (canMove b) $ options
step (PM mbs) = PM $ fmap (second step) mbs
step (PP pbs) = PP $ fmap (second step) pbs

level :: Int -> Board -> ProbBoard
level n b = iterate step (PL b)  !! n

alphaBeta' ::  Double -> Double -> ProbBoard -> Double
alphaBeta' _ _ (PL b) = score b
alphaBeta' alpha beta (PM mbs) = case mbs of
  ((_, pb):pbs) -> let v = max maxbound' $ alphaBeta' alpha beta pb
                       alpha' = max alpha v
            in if beta <= alpha' || null pbs
               then v else alphaBeta' alpha' beta (PM pbs)
  [] -> 0
alphaBeta' alpha beta (PP pbs) = case pbs of
  ((_, pb):pbs') -> let v = min minbound' $ alphaBeta' alpha beta pb
                        beta' = min beta v
            in if beta' <= alpha || null pbs
               then v else alphaBeta' alpha beta' (PP pbs')
  [] -> 0

alphaBeta :: ProbBoard -> Double
alphaBeta (PL b) = score b
alphaBeta pb     = alphaBeta' maxbound' minbound' pb

alphaBetaMove' :: ProbBoard -> Maybe Move
alphaBetaMove' (PM mbs) = listToMaybe . map fst . sortBy
  (comparing $ alphaBeta . snd) $ mbs
alphaBetaMove' _ = error "error game tree"

alphaBetaMove :: Board -> Maybe Move
alphaBetaMove b = alphaBetaMove' (level 3 b)

exptScore = [4^15,4^14,4^13,4^12,4^8,4^9,4^10,4^11,4^7,4^6,4^5,4^4,4^0,4^1,4^2,4^3]

heurScore :: Board -> Int
heurScore board = maximum
  [sum $ zipWith (*) exptScore (join $ _cells board),
   sum $ zipWith (*) exptScore (join $ transpose $_cells board),
   sum $ zipWith (*) exptScore (join $ map reverse $_cells board),
   sum $ zipWith (*) exptScore (join . map reverse . transpose $_cells board)]

exptProbBoard :: ProbBoard -> Maybe Double
exptProbBoard (PL b)   = Just . fromInteger . toInteger $ heurScore b
exptProbBoard (PM [])  = Nothing
exptProbBoard (PM mbs) = maximum . map (exptProbBoard . snd) $ mbs
exptProbBoard (PP [])  = Nothing
exptProbBoard (PP pbs) = Just $ sum . map (\(p,b)-> p * fromMaybe 0 (exptProbBoard b)) $ pbs

exptMove' :: ProbBoard -> Maybe Move
exptMove' (PM mbs) = listToMaybe . map fst . sortBy
  (comparing $ exptProbBoard . snd) $ mbs
exptMove' _ = error "error game tree"

exptMove :: Board -> Maybe Move
exptMove b = exptMove' (level 3 b)

maxbound', minbound' :: Double
maxbound' = -100000000
minbound' = 100000000

alphaBetaPlayer :: Player
alphaBetaPlayer = Player (PlayerName "AlphaBeta-AI") $ return . alphaBetaMove

exptPlayer :: Player
exptPlayer = Player (PlayerName "Expt-AI") $ return . exptMove

score :: Board -> Double
score b = fromIntegral $ 100000 + orderScore + closePairsScore  + maxAtEndScore + emptyScore - divergeScore -- - total * 10 + emptyScore
  where
    emptyScore = 4000 * en
    total = sum (map sum (_cells b))
    stat = toStat b
    en = emptyNum stat
    log2Int :: Int -> Int
    log2Int = floor . (5^) . floor .  logBase 2.0 . fromIntegral
    orderScore = 8 * sum (map log2Int (ordered stat))
    maxAtEndScore = 5 * sum (map log2Int (maxAtEnd stat))
    closePairsScore = 2 * sum (map log2Int cp) -  400 * length cp
    divergeScore = 10 * sum (map (8^) dv) + 800 * length dv
    cp = closePairs stat
    dv = diverge stat
