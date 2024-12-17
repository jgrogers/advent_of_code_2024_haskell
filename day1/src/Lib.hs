module Lib
    ( someFunc, diff, cmp, count, nth
    ) where

import System.IO(Handle, IOMode(ReadMode), openFile, hGetLine, hPutStr, hClose, hIsEOF, stderr)
import Data.List(transpose)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

stringToInt :: String -> Int
stringToInt s = read s

diff :: (Int, Int) -> Int
diff ab = do
  let a = fst ab
  let b = snd ab
  abs(a-b)

cmp :: Eq a=> a -> a -> Int
cmp a b = do
  if a == b then 1
  else 0

count :: Eq a => a -> [a] -> Int
count a lst = do
  let e = map (cmp a) lst
  sum e

nth :: Int -> [a] -> a
nth _ [] = error "nth: empty list"
nth 0 (x:_) = x
nth n (_:xs) = nth (n - 1) xs

loadDataCols :: String -> IO [a]
loadDataCols filename = do
    fileStrings <- readFile filename
    let fileLines = lines fileStrings
    let fileWords = map words fileLines
    let fileInts = map (map stringToInt) fileWords
    transpose fileInts