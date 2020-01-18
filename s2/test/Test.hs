{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude
import S2.Church

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec do
  describe "Church" do
    prop "Num" testNums
    prop "Cons" $ \(a :: Int) b ->
      (first (cons a b) `shouldBe` a) .&&. (second (cons a b) `shouldBe` b)

testNums :: NonNegative (Small Integer)
         -> NonNegative (Small Integer)
         -> Expectation
testNums (NonNegative (Small a)) (NonNegative (Small b)) =
  fromInteger a + fromInteger b `shouldBe` (fromInteger (a + b) :: ChurchNum Int)
