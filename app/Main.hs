module Main where

import System.IO
import Lib

main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  run
