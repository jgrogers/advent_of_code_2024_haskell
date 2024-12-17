module Main (main) where

import Lib

main :: IO ()
main = do
  part1 <- getPart1Ans "data/input.txt"
  print ("The answer to part 1 is " ++ show part1)
  part2 <- getPart2Ans "data/input.txt"
  print ("The answer to part 2 is " ++ show part2)
