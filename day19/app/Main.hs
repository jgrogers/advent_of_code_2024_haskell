module Main (main) where

import Lib

main :: IO ()
main = do
  num_ways <- testPart2 "data/input.txt"
  print (show num_ways)
  return ()