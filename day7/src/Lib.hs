module Lib
    ( someFunc
    ) where
import Data.List.Split
import Data.List
import Debug.Trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

convertFromStringPair :: (String, String) -> (Int,[Int])
convertFromStringPair x = do
    let readInt y = read y ::Int
    let outpval = readInt $ fst x
    let operandsStrs = words $ snd x
    let operands = map readInt operandsStrs
    (outpval, operands)

loadData :: String -> IO [(Int, [Int])]
loadData filename = do
    inputText <- readFile filename
    let inputLines = lines inputText
    let calibrations = map (convertFromStringPair . splitAtFirst ':') inputLines
    return calibrations

operate :: Int -> (Int->Int->Int, Int) -> Int
operate antecedent oppair = do
    let (operation, opx) = oppair
    operation antecedent opx

evaluateOperatorAssignment :: Int -> [Int] -> [Int->Int->Int] -> Bool
evaluateOperatorAssignment calib operands operators = do
    let antecedent = head operands
    let operations = zip operators (tail operands) 
    let res = foldl operate antecedent operations
    calib == res

makeCheckAssignment :: Int -> [Int] -> [Int->Int->Int] ->[Int->Int->Int]-> Bool
makeCheckAssignment calib operands operators operations = do -- Check the assignment when length operators is one less than length operands
    if length operators == length (tail operands) then evaluateOperatorAssignment calib operands operators
    else do
        let new_operators = map (\op -> operators++[op]) operations
        let res = map (\op -> makeCheckAssignment calib operands op operations) new_operators
        foldl (||) False res

checkOperatorAssignmentExists :: (Int, [Int]) -> [Int->Int->Int]-> Bool
checkOperatorAssignmentExists inp operations = do
    let calib = fst inp
    let operands = snd inp
    let res = makeCheckAssignment calib operands [] operations
    trace (show res) res
    

getSumValidCalib :: String -> [Int->Int->Int] -> IO Int
getSumValidCalib filename operations = do
    calibrations <- loadData filename
    let res = map (`checkOperatorAssignmentExists` operations) calibrations
    let resPairs = zip res (map fst calibrations)
    let optSum x (opt, y) = if opt then x + y else x
    let filteredsum = foldl optSum 0 resPairs
    return filteredsum

runPart1 :: IO Int
runPart1 = do
    let operations = [(+), (*)]
    getSumValidCalib "data/input.txt" operations

testPart1 :: IO Int
testPart1 = do
    let operations = [(+), (*)]
    getSumValidCalib "data/test.txt" operations

concatInt :: Int->Int->Int
concatInt x y = do
    let sX = show x
    let sY = show y
    let sXsY = sX++sY
    read sXsY :: Int

runPart2 :: IO Int
runPart2 = do
    let operations = [(+), (*), concatInt]
    getSumValidCalib "data/input.txt" operations

testPart2 :: IO Int
testPart2 = do
    let operations = [(+), (*), concatInt]
    getSumValidCalib "data/test.txt" operations
