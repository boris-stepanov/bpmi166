{-# LANGUAGE BlockArguments #-}

import Prelude    hiding (foldl, foldr, foldl')
import S3.Basic

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec do
  describe "Folds" do
    it "foldr" $ foldr (+) 5 [1,2,3] === 11
    it "foldl" $ foldl (+) 5 [1,2,3] === 11
    it "foldl'" $ foldl' (+) 5 [1,2,3] === 11
    prop "all together" testAllTogether

testAllTogether :: [Small Int] -> Small Int -> Property
testAllTogether list initial = right === left .&&. left === left'
  where
  right = foldr (+) initial list
  left = foldl (+) initial list
  left' = foldl' (+) initial list
