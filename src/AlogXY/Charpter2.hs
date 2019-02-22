module AlogXY.Charpter2 where

import           AlogXY.Charpter1

insertForSort :: Ord a => [a] -> a -> [a]
insertForSort [] a = [a]
insertForSort (x:xs) a
  | x < a = x : insertForSort xs a
  | otherwise = a:x:xs

insertSort :: Ord a => [a] -> [a]
insertSort = foldl insertForSort []

insertTreeSort :: Ord a => [a] -> [a]
insertTreeSort = inOrder . foldl addToBST Empty . fromList
