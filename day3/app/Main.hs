module Main (main) where

import Lib

main :: IO ()
main = do
  inputText <- readFile "data/input.txt"
  let regexStr = "mul\\([0-9]+,[0-9]+\\)"
  let output = matchRegex inputText regexStr
  let mulOperands = map getMulNumsRegex output
  let day1_ans = sum (map (foldl (*) 1) mulOperands)
  print ("Part 1 answer is " ++ show day1_ans)

  inputText <- readFile "data/input.txt"
  let regexStr = "(do\\(\\))|(don't\\(\\))|(mul\\([0-9]+,[0-9]+\\))"
  let output = matchRegex inputText regexStr
  print output
  let stripped = removeDisabledMuls output
  print stripped
  let mulOperands = map getMulNumsRegex stripped
  let day2_ans = sum (map (foldl (*) 1) mulOperands)
  print ("Part 2 answer is " ++ show day2_ans)

  