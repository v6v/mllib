-- file Spec.hs
import           Control.Exception (evaluate)
import           DecisionTree      as DT
import           Test.Hspec
import           Test.QuickCheck

e1,e2,e3 :: DT.Example
e1 = Example [1,2] (Label "A")
e2 = Example [2,2] (Label "B")
e3 = Example [2,3] (Label "B")

main :: IO ()
main = hspec $ do
  describe "DecisionTree" $ do
    it "choose the feature that get the most information gain" $ do
      DT.choose [e2, e1, e3] [0, 1] `shouldBe` (0 :: Int)
    it "predit should equal" $ do
      DT.buildTree [e2, e1, e3] [0, 1] e1 `shouldBe` DT.extract e1
      DT.buildTree [e2, e1, e3] [0, 1] e2 `shouldBe` DT.extract e2
      DT.buildTree [e2, e1, e3] [0, 1] e3 `shouldBe` DT.extract e3
  describe "read" $ do
    context "when used with ints" $ do
      it "is inverse to show" $ property $
        \x -> (read . show) x == (x :: Int)
