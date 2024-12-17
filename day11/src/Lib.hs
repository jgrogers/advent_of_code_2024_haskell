module Lib
    ( someFunc
    ) where
import System.Directory (setOwnerSearchable)

import Data.Map (Map)
import qualified Data.Map
someFunc :: IO ()
someFunc = putStrLn "someFunc"

readInt :: String -> Integer
readInt s = do
    let x = read s ::Integer
    x

hasEvenDigits :: Integer -> Bool
hasEvenDigits x = do 
    let strX = show x
    even $ length strX

splitStone :: Integer -> [Integer]
splitStone x = do
    let strX = show x
    let lenStrX = length strX
    let halfLen = lenStrX `div` 2
    [readInt $ take halfLen strX, readInt $ drop halfLen strX]

blink :: [Integer] -> [Integer]
blink stones = do
    if null stones then []
    else do
        let headStone = head stones
        if headStone == 0 then 1:blink(tail stones)
        else if hasEvenDigits headStone then splitStone headStone ++ blink (tail stones)
        else (headStone * 2024):blink (tail stones)
            
blinkFast' :: [(Integer, Integer)] -> Map Integer Integer -> [(Integer, Integer)]
blinkFast' stones currNewMap =
    if null stones then Data.Map.toList currNewMap
    else do
        let (headStone, cnt) = head stones
        if headStone == 0 then blinkFast' (tail stones) (Data.Map.insertWith (+) 1 cnt currNewMap)
        else if hasEvenDigits headStone then do
            let [newstone1, newstone2] = splitStone headStone
            let newNewMap = Data.Map.insertWith (+) newstone1 cnt (Data.Map.insertWith (+) newstone2 cnt currNewMap)
            blinkFast' (tail stones) newNewMap
        else blinkFast' (tail stones) (Data.Map.insertWith (+) (2024*headStone) cnt currNewMap)

blinkFast :: [(Integer, Integer)] -> [(Integer, Integer)]
blinkFast stones = blinkFast' stones Data.Map.empty


lengthAfterBlinkN :: Integer -> Int -> Int
lengthAfterBlinkN stone n = do
    length (iterate blink [stone] !! n)

loadData :: String -> IO [Integer]
loadData filename = do
    fileStrs <- readFile filename
    let fileWords = words fileStrs
    let myArray = map readInt fileWords
    return (myArray)


blinkN :: [Integer] -> Int -> [Integer]
blinkN stones n = iterate blink stones !! n

blinkNLen :: [Integer] -> Int -> Int
blinkNLen stones n = do
    let part2_sublens = map (\x -> lengthAfterBlinkN x n) stones
    let s = sum part2_sublens
    s

doPart1 :: IO Int
doPart1 = do
    stones <- loadData "data/input.txt"
    let part1 = length (iterate blink stones !! 25)
    print ("The answer to part 1 is " ++ show part1)
    return part1

doPart1Fast :: IO Integer
doPart1Fast = do
    stones <- loadData "data/input.txt"
    let stonesCnt = zip stones (repeat 1)
    let part1terms = iterate blinkFast stonesCnt !! 25
    let part1 = sum $ map snd part1terms
    print ("The answer to part1 fast is " ++ show part1)
    return part1

doPart2 :: IO Integer
doPart2 = do
    stones <- loadData "data/input.txt"
    let stonesCnt = zip stones (repeat 1)
    let part2terms = iterate blinkFast stonesCnt !! 75
    let part2 = sum $ map snd part2terms
    print ("The answer to part2 is " ++ show part2)
    return part2

doPart2Fast :: IO Int
doPart2Fast = do
    stones <- loadData "data/input.txt"
    let part2_sublens = map (\x -> lengthAfterBlinkN x 75) stones
    let s = sum part2_sublens
    print ("The fast answer to part 2 is " ++ show s)
    return s
