module Main (main) where

import Lib

main :: IO ()
main = do
  part1 <- runPart1
  print("The answer to part 1 is " ++ show part1)
  part2 <- runPart2
  print ("The answer to part 2 is " ++ show part2)
