module Main (main) where
import System.IO(Handle, IOMode(ReadMode), openFile, hGetLine, hPutStr, hClose, hIsEOF, stderr)
import Control.Monad(unless)
import Data.List(transpose, sort)
import Data.Typeable
import Text.Read (read)
import Debug.Trace
import Lib(checkReport1, checkReport2, count)

stringToInt :: String -> Int
stringToInt = read


main :: IO ()
main = do
  reportsString <- readFile "data/input.txt"
  let reportsStrings = lines reportsString
  let reportsWords = map words reportsStrings
  let reports = map (map stringToInt) reportsWords
  let safe = map checkReport1 reports
  let numSafePart1 = count safe
  print ("The answer to part 1 is " ++ show numSafePart1)
  let safe = map checkReport2 reports
  let numSafePart2 = count safe
  print ("The answer to part 2 is " ++ show numSafePart2)
  let testlist = take 5 (iterate (+ 1) 0)
  print (show testlist)
