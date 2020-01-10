module Hello.Lib (
   callPrint
 ) where

import Prelude

callPrint :: String -> IO ()
callPrint = putStrLn
