module DecisionTree(gain
                   ,Example(..)
                   ,Label(..)
                   ,choose
                   ,judge
                   ,extract
                   ,buildTree) where

import           Data.Function
import           Data.List     (maximumBy)
import           Data.Map      (fromListWith, toList)
import           Data.Ord      (comparing)

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

type Feature = Integer
type Features = [Feature]
type Possibility = (Label,Double)
type Entropy = Double
newtype Label = Label String deriving (Eq, Ord, Show)
data Example = Example Features Label

type DecisionTree = Example -> Label
type RemainingFeatures = [Int]

threshold :: Double
threshold = 0.9

buildTree :: [Example] -> RemainingFeatures -> DecisionTree
buildTree es rf (Example fs _) = fst . maximumBy (comparing snd) $ possibilities
  where best = choose es rf
        feature = fs !! best
        possibilities = group $ filter (\(Example fs' _) -> fs' !! best == feature) es


buildSubTree :: [Example] -> RemainingFeatures -> [DecisionTree]
buildSubTree = undefined

sayTree :: DecisionTree -> String
sayTree = undefined

judge :: DecisionTree -> Example -> Label
judge dt = dt

choose :: [Example] -> [Int] -> Int
choose es ls = fst . maximumBy (comparing snd) $ featureGain
  where featureGain = map (\i -> (i, gain es i)) ls

gain :: [Example] -> Int -> Double
gain es i = entropy es - sum (map (uncurry (*)) entropyDistr)
  where
    distr = frequency $ map (extractFeatures i) es
    total = sum $ map snd distr
    partialEntropy :: (Feature, Int) -> (Double, Double)
    partialEntropy (f,t) = (fromIntegral t/fromIntegral total , entropy (filter (\(Example fs _) -> (fs !! i) == f) es))
    entropyDistr = map partialEntropy distr

entropy :: [Example] -> Entropy
entropy = sum . map ((\p -> - p * logBase 2 p) . snd) . group

group :: [Example] -> [Possibility]
group es = map possibility distribution
  where distribution = frequency $ map extract es
        total = fromIntegral $ length es
        possibility (f, c)= (f, fromIntegral c / total)
  -- valueList

extract :: Example -> Label
extract (Example _ l) = l

extractFeatures :: Int -> Example -> Feature
extractFeatures i (Example fs _) = fs !! i
