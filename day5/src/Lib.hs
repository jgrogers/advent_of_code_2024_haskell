module Lib
    ( loadData
    ) where

import Data.List.Split
import Data.List

splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

convertPairStringsPairInt :: (String, String) -> (Int, Int)
convertPairStringsPairInt x = do
    let part1 = fst x
    let part2 = snd x
    let int1 = read part1 :: Int
    let int2 = read part2 :: Int
    (int1, int2)

convertListStringsListInts :: [String] -> [Int]
convertListStringsListInts x = do
    let readInt x = read x ::Int
    map readInt x
loadData :: String -> IO ([(Int,Int)], [[Int]])
loadData filename = do
    inputText <- readFile filename
    let inputLines = lines inputText
    let ruleStrings = filter (elem '|') inputLines
    let orderingStrings = filter (elem ',') inputLines
    let rules = map convertPairStringsPairInt (map (splitAtFirst '|') ruleStrings)
    let orderings = map convertListStringsListInts (map (splitOn ",") orderingStrings)
    return (rules, orderings)
    
checkRule :: (Int, Int) -> [Int] -> Bool
checkRule rule ordering = do
    let indexBefore = elemIndex (fst rule) ordering
    let indexFollow = elemIndex (snd rule) ordering
    case (indexBefore, indexFollow) of
        (Just x, Just y) -> x<y
        (_,_) -> True

checkRules :: [(Int, Int)] -> [Int] -> Bool
checkRules rules ordering = do
    foldl (&&) True (map (\x -> checkRule x ordering) rules)

getMiddleElement :: [Int] -> Int
getMiddleElement ordering = do
    let ind = length ordering `div` 2
    ordering !! ind

checkAndGetValue :: [(Int,Int)] -> [Int] -> Int
checkAndGetValue rules ordering = do
    if checkRules rules ordering then getMiddleElement ordering else 0

swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt a another list = list1 ++ [list !! another] ++ list2 ++ [list !! a] ++ list3
    where   list1 = take a list;
            list2 = drop (succ a) (take another list);
            list3 = drop (succ another) list

getPart1Ans :: [(Int, Int)] -> [[Int]] -> Int
getPart1Ans rules orderings = do
    sum (map (\x -> checkAndGetValue rules x) orderings)

swapViolatedRule :: (Int,Int) -> [Int] -> [Int]
swapViolatedRule rule ordering = do
    if checkRule rule ordering then ordering
    else do
        let indexFst = elemIndex (fst rule) ordering
        let indexSnd = elemIndex (snd rule) ordering
        case (indexFst, indexSnd) of
            (Just x, Just y) -> do
                let minxy = min x y
                let maxxy = max x y
                swapElementsAt minxy maxxy ordering
            (_,_) -> ordering

reorderByRules :: [(Int, Int)] -> [Int] -> [Int]
reorderByRules rules ordering = do
    if checkRules rules ordering then ordering
    else do
        let violatedRules = filter (\x -> not $ checkRule x ordering) rules
        let newOrdering = swapViolatedRule (violatedRules!!0) ordering
        reorderByRules rules newOrdering
        

reorderFailByRules :: [(Int, Int)] -> [[Int]] -> [[Int]]
reorderFailByRules rules orderings = do
    let filtered = filter (\x -> not $ checkRules rules x) orderings
    let reorderedRules = map (reorderByRules rules) filtered
    reorderedRules
    
getPart2Ans :: [(Int, Int)] -> [[Int]] -> Int
getPart2Ans rules orderings = do
    let fixed = reorderFailByRules rules orderings
    sum (map (\x -> checkAndGetValue rules x) fixed)

