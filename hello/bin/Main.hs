#!/usr/bin/env stack
-- stack runghc --package base

module Main (
   main
 ) where

import Prelude
import Hello.Lib

main :: IO ()
main = callPrint "Hello world!"
