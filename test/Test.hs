{-# LANGUAGE BlockArguments #-}

import Prelude    hiding (foldl, foldr, foldl')
import Basic

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec do
  describe "Functor" do
    it "Identity" _
    it "Function" _
    it "Pair" _
    it "Tree" _
  describe "Applicative" _
  describe "Monad" _
