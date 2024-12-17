module Lib
    ( count, checkReport1, checkReport2, checkLevelDec, checkLevelInc, checkReportIncDec, splitAtRemove, checkLevelsDampener, checkLevelsDampenerInd
    ) where
import System.IO(Handle, IOMode(ReadMode), openFile, hGetLine, hPutStr, hClose, hIsEOF, stderr)
import Control.Monad(unless)
import Data.List(transpose, sort)
import Data.Typeable
import Text.Read (read)
import Debug.Trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

checkLevelInc :: Int -> [Int] -> Bool
checkLevelInc level rest | length rest < 1 = True
                      | otherwise = do
                        let (nextLevel:rest2) = rest
                        nextLevel - level > 0 && nextLevel - level < 4 && checkLevelInc nextLevel rest2

checkLevelDec :: Int -> [Int] -> Bool
checkLevelDec level rest | length rest < 1 = True
                      | otherwise = do
                        let (nextLevel:rest2) = rest
                        level - nextLevel > 0 && level - nextLevel < 4 && checkLevelDec nextLevel rest2



checkReportIncDec :: [Int] -> Bool
checkReportIncDec report = do
  checkLevelInc (head report) (tail report) || checkLevelDec (head report) (tail report)

splitAtRemove :: Int -> [a] -> [a]
splitAtRemove n xs = do 
  let parts = splitAt n xs
  let newtail = tail (snd parts)
  fst parts ++ newtail

checkLevelsDampenerInd :: [Int] -> Int -> Bool
checkLevelsDampenerInd report ind = do
  let dampened = splitAtRemove ind report
  checkReportIncDec dampened

checkLevelsDampener :: [Int] -> Bool
checkLevelsDampener report = do
  let inds = take (length report) (iterate (+ 1) 0)
  foldl (||) False (map (checkLevelsDampenerInd report) inds)

checkReport1 :: [Int] -> Bool
checkReport1 report = do
  checkReportIncDec report

checkReport2 :: [Int] -> Bool
checkReport2 report = do
  checkReportIncDec report || checkLevelsDampener report

count :: [Bool] -> Int
count = foldl (\acc p -> if p then acc+1 else acc) 0
