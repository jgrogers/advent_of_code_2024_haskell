module Main (main) where

import System.IO(Handle, IOMode(ReadMode), openFile, hGetLine, hPutStr, hClose, hIsEOF, stderr)
import Control.Monad(unless)
import Data.List(transpose, sort)
import Data.Typeable
import Text.Read (read)
import Debug.Trace
import Lib(diff, cmp, count)
stringToInt :: String -> Int
stringToInt s = read s

loadFile :: Handle -> Integer -> IO ()
loadFile handle lineNumber = do
  end <- hIsEOF handle
  unless end $ do
    line <- hGetLine handle
    putStrLn $ ":" ++ show lineNumber ++ ": " ++ line
    loadFile handle $ lineNumber + 1

--diff :: (Int, Int) -> Int
--diff ab = do
--  let a = fst ab
--  let b = snd ab
--  abs(a-b)
--
--cmp :: Eq a=> a -> a -> Int
--cmp a b = do
--  if a == b then 1
--  else 0
--
--count :: Eq a => a -> [a] -> Int
--count a lst = do
--  let e = map (cmp a) lst
--  sum e
--
--nth :: Int -> [a] -> a
--nth _ [] = error "nth: empty list"
--nth 0 (x:_) = x
--nth n (_:xs) = nth (n - 1) xs



main :: IO ()
main = do
  locations <- readFile "data/input.txt"
--  locations <- readFile "data/test.txt"
--  let locs = lines locations
--  let wrd = map words locs
--  let wrdInts = transpose (map (map stringToInt) wrd)
  wrdInts <- loadDataCols "data/input.txt"
  let colA = sort (wrdInts !! 0)
  let colB = sort (wrdInts !! 1)
  let pairs = zip colA colB
  let diffs = map diff pairs
  let s = sum diffs
  print ("The answer to part 1 is " ++ show s)
  let dup_count = map (`count` colB) colA
  print dup_count
  let additional = zipWith (*) dup_count colA
--  print additional
  print (sum additional)
  print ("The answer to part 2 is " ++ show (sum additional))
  

