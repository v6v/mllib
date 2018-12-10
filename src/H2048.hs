module H2048 where

import           Control.Lens
import           Data.List          (maximumBy, transpose)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.List.Ordered  (isSorted)
import           System.Random      (randomRIO)
import           Data.Function(on)



newtype ColSize = ColSize Int deriving Show
newtype RowSize = RowSize Int deriving Show

data Board = Board
  { _colSize :: ColSize
  , _rowSize :: RowSize
  , _cells   :: [[Int]]
  , _pos     :: [[(Int, Int)]]
  }
instance Show Board where
  show Board{_cells=cells} = unlines $ fmap format cells
    where format = unwords . fmap formatNum
          formatNum n = let ns = show n
                            l = length ns
                            toFill | l < 4 = 4-l
                                   | otherwise = l
                        in ns ++ replicate toFill ' '
instance Eq Board where
  Board{_cells=c1} == Board{_cells=c2} = c1 == c2

data Move = UP | DOWN | LEFT | RIGHT deriving (Show, Eq, Read)

makeBoard :: ColSize -> RowSize -> Board
makeBoard c@(ColSize col) r@(RowSize row) = Board c r board' pos'
  where
    board' = replicate row . replicate col $ 0
    pos' = fmap (\i -> fmap ((,)i) [0..col-1]) [0..row-1]

pickRandom :: NonEmpty a -> IO a
pickRandom xs = (xs NE.!!) <$> randomRIO (0, NE.length xs - 1)

pickRandom' :: [a] -> IO (Maybe a)
pickRandom' xs = (\i -> xs ^? ix i) <$> randomRIO (0, length xs - 1)

randomValue :: IO Int
randomValue = pickRandom $ NE.fromList [2, 2, 2, 2, 2, 2,  2, 2, 2, 4]

emptyPos :: Board -> [(Int, Int)]
emptyPos Board {_cells = cells, _pos = pos} =
  concat $ map fst . filter (\(_, v) -> v == 0) <$> zipWith zip pos cells

addRandom :: Board -> IO (Maybe Board)
addRandom b@Board {_cells = cells} = do
  v <- randomValue
  fmap (\(c,r)->b {_cells = cells & ix c . ix r .~ v}) <$> randomPos b
  where
    randomPos = pickRandom' . emptyPos

squeeze :: [Int] -> [Int]
squeeze line = valued ++ replicate toFill 0
  where
    l = length line
    valued = filter (/= 0) line
    toFill = l - length valued

merge :: [Int] -> [Int]
merge [] = []
merge [a] = [a]
merge (a:b:xs)
  | a == b = a + b : 0 : merge xs
  | otherwise = a : merge (b : xs)

rotate270 :: Board -> Board
rotate270 b@Board{_cells=cells} = b {_cells = rotate' cells}
  where rotate' = reverse . transpose

rotate90 :: Board -> Board
rotate90 b@Board{_cells=cells} = b {_cells = rotate' cells}
  where rotate' = transpose . reverse

rotate180 :: Board -> Board
rotate180 = rotate90 . rotate90

moveLeft :: Board -> Board
moveLeft b@Board{_cells=cells} = b {_cells = fmap (squeeze . merge. squeeze) cells}

move :: Move -> Board -> Board
move LEFT  = moveLeft
move DOWN  = rotate270 . moveLeft . rotate90
move UP    = rotate90 . moveLeft . rotate270
move RIGHT = rotate180 . moveLeft . rotate180

data Stat = Stat
    { emptyNum   :: Int
    , ordered    :: [Int]
    , maxAtEnd   :: [Int]
    , closePairs :: [Int]
    , diverge :: [Int]
    }

toStat :: Board -> Stat
toStat b@Board{_cells = cells} =
    Stat
    { emptyNum = sum $ map (length . filter (== 0)) cells
    , ordered = orderer'
    , maxAtEnd = maxAtEnd
    , closePairs = filter (/= 0) closePairs
    , diverge = diverge
    }
  where
    maxAtEnd = maxAtEnd' b ++
               maxAtEnd' (rotate90 b) ++
               maxAtEnd' (rotate180 b) ++
               maxAtEnd' (rotate270 b)
    maxAtEnd' = filter (/=0) .
                  map (\l@(a:_)-> if a == maximum l then a else 0) .
                  _cells
    orderer' = maximumBy (compare `on` sum) [
        orderT id ++ orderT rotate90,
        orderT id ++ orderT rotate270,
        orderT rotate180 ++ orderT rotate270,
        orderT rotate180 ++ orderT rotate90
        ]
    orderT t = filter (/= 0). map maximum $ filter isSorted (_cells (t b))
    closePairs = concatMap closePairLine cells   ++
                 concatMap closePairLine (reverse . transpose $cells)
    closePairLine []       = []
    closePairLine [a]      = []
    closePairLine (a:b:xs) = if max a b == 2 * min a b
      then a:closePairLine (b:xs)
      else closePairLine (b:xs)
    diverge = (concatMap divergeLine cells) ++ (concatMap divergeLine (reverse.transpose $ cells))
    divergeLine :: [Int] -> [Int]
    divergeLine []       = []
    divergeLine [a]      = []
    divergeLine (a:b:xs)
      | a == 0 || b == 0 || d == 0 = divergeLine (b:xs)
      | otherwise = d:divergeLine (b:xs)
      where
        d :: Int
        d = abs $ floor (logBase 2.0 (fromIntegral (a+1)) - logBase 2.0 (fromIntegral (b+1)))
