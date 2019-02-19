module AlogXY.Charpter1 where

import           Control.Monad (replicateM)
import           Data.Function (on)
import           Data.List     (elemIndex, partition)
import           Data.List     (sortBy)
import           Data.Maybe    (fromJust, fromMaybe)
import           System.Random (randomRIO)

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Eq)

-- | Exercise 1.1: given pre-order list and in-order list, get back Tree
--
-- >>> let t = genTree [1,2,4,3,5,6] [4,2,1,5,3,6]
-- >>> preOrder t
-- [1,2,4,3,5,6]
-- >>> inOrder t
-- [4,2,1,5,3,6]
-- >>> postOrder t
-- [4,2,5,6,3,1]
genTree :: (Eq a) => [a] -> [a] -> Tree a
genTree [] _ = Empty
genTree (pv:ps) is = Node (genTree pleft ileft) pv (genTree pright iright)
  where (ileft, _:iright) = splitAt index is
        (pleft, pright) = splitAt index ps
        index = fromMaybe (-1) $ elemIndex pv is

preOrder :: Tree a -> [a]
preOrder Empty               = []
preOrder (Node left v right) = v : (preOrder left ++ preOrder right)

inOrder :: Tree a -> [a]
inOrder Empty               = []
inOrder (Node left v right) = inOrder left ++ [v] ++ inOrder right

postOrder :: Tree a -> [a]
postOrder Empty               = []
postOrder (Node left v right) = postOrder left ++ postOrder right ++ [v]

-- | Exercise 1.2: traverse a BST for value in range
--
-- >>> let t = genTree [1,2,4,3,5,6] [4,2,1,5,3,6]
-- >>> traverseInRange 2 4 t (+1)
traverseInRange :: Ord a => a -> a -> Tree a -> (a -> b) -> Tree b
traverseInRange _ _ Empty _ = Empty
traverseInRange min' max' (Node left v right) f
  | v < min' = traverseInRange min' max' right f
  | v > max' = traverseInRange min' max' left f
  | otherwise = Node (traverseInRange min' max' left f)
                (f v)
                (traverseInRange min' max' right f)

-- | Exercise 1.3: delete one value from BST , and then the node has two
--   non-nil children, using the max of left children as new node.
-- >>> let t = genTree [2,1,3,5,4,6] [1,2,3,4,5,6]
-- >>> delete 2 t
delete :: Ord a => a -> Tree a -> Tree a
delete _ Empty = Empty
delete t (Node left v right)
  | t < v = Node (delete t left) v right
  | t > v = Node left v (delete t right)
  | left == Empty = right
  | right == Empty = left
  | otherwise = Node (delete max' left) max' right
    where max' = fromJust $ maxInBST left

maxInBST :: Ord a => Tree a -> Maybe a
maxInBST Empty            = Nothing
maxInBST (Node _ v Empty) = Just v
maxInBST (Node _ _ right) = maxInBST right

minInBST :: Ord a => Tree a -> Maybe a
minInBST Empty            = Nothing
minInBST (Node Empty v _) = Just v
minInBST (Node left _ _)  = minInBST left

-- | Exercise 1.4: randomly build from list to BST, in order to decrease the
--   possibility getting a unbalanced binary tree.
-- >>> randFromList [2,1,3,5,4,6]
randFromList :: Ord a => [a] -> IO (Tree a)
randFromList xs = do
  ys <- replicateM (length xs) $ randomRIO (1 :: Int, 100000)
  return . fromList $ map fst $ sortBy (compare `on` snd) (zip xs ys)

fromList :: Ord a => [a] -> Tree a
fromList [] = Empty
fromList (a:as) = Node (fromList left) a (fromList right)
  where (left, right) = partition (<a) as
