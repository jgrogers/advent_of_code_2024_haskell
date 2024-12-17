module Main (main) where

import Lib

main :: IO ()
main = do
  datablock <- loadData "data/input.txt"
  let part1 = countAllXmasses datablock
  print ("The answer to part 1 is " ++ show part1)
  let part2 = countAllX_masses datablock
  print ("The answer to part 2 is " ++ show part2)

