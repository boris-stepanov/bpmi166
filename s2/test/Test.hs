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
    prop "Num" $ \(Small a) b ->
      (fromInteger a :: ChurchNum Int) + fromInteger b `shouldBe` fromInteger (a + b)
    prop "Cons" $ \(Small (a :: Int)) b ->
      (first (cons a b) `shouldBe` a) .&&. (second (cons a b) `shouldBe` b)
