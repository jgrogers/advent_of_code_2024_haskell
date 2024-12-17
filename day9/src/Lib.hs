module Lib
    ( someFunc
    ) where
import Debug.Trace
import Data.IntMap (IntMap)
someFunc :: IO ()
someFunc = putStrLn "someFunc"

loadData :: String -> IO [Int]
loadData filename = do
    rawData <- readFile filename
    let readInt = read . (:"") 
    
    return (map readInt rawData)

convertDataFSToBlocks :: [Int] -> Int -> [Int]
convertDataFSToBlocks inp val = do
    let cnt = head inp
    case tail inp of
        [] -> replicate cnt (-1)
        _ -> replicate cnt (-1) ++ convertDataToBlocks (tail inp) val


convertDataToBlocks :: [Int] -> Int -> [Int]
convertDataToBlocks inp val = do
    let cnt = head inp
    case tail inp of
        [] -> replicate cnt val
        _ -> replicate cnt val ++ convertDataFSToBlocks (tail inp) (val + 1)

squashFS :: [Int] -> [Int]
squashFS = filter (/= (-1)) 

checkRunNotEmpty :: (Int, Int) -> Bool
checkRunNotEmpty a = do
    let (b, c) = a
    b /= (-1)

squashRunFS :: [(Int, Int)] -> [(Int, Int)]
squashRunFS = filter checkRunNotEmpty

convertDataFSToRun :: [Int] -> Int -> [(Int, Int)]
convertDataFSToRun inp val = do
    let cnt = head inp
    case tail inp of
        [] -> if cnt == 0 then [] else [(-1, cnt)]
        _ -> if cnt == 0 then convertDataToRun (tail inp) val else (-1, cnt):convertDataToRun (tail inp) val

convertDataToRun :: [Int] -> Int -> [(Int, Int)]
convertDataToRun inp val = do
    let cnt = head inp
    case tail inp of
        [] -> if cnt == 0 then [] else [(val, cnt)]
        _ -> if cnt == 0 then convertDataFSToRun (tail inp) (val+1) else (val, cnt):convertDataFSToRun (tail inp) (val+1)

doPart1Stuff :: [Int] -> [Int] -> [Int]
doPart1Stuff inp rev = do
    if null $ tail inp  then
        case head inp of
            (-1) -> [head rev]
            _ -> [head inp]
    else case head inp of
        (-1) -> head rev : doPart1Stuff (tail inp) (tail rev)
        _ -> head inp : doPart1Stuff (tail inp) rev

mulpair ::(Int, Int) -> Int
mulpair a = do
    let (b,c) = a
    b*c

getPart1Ans :: IO ()
getPart1Ans = do
    inp <- loadData "data/input.txt"
    let startingFS = convertDataToBlocks inp 0
    print startingFS
    let revStartingFS = squashFS $ reverse startingFS
    print revStartingFS
    let count_nonempty a b = if a /= (-1) then b+1 else b
    --let num_nonempty = foldl count_nonempty 0 startingFS
    let num_nonempty = length $ filter (/= (-1)) startingFS
    print num_nonempty
    let filled = doPart1Stuff startingFS revStartingFS
    let finalFS = take num_nonempty filled
    print (finalFS)
    let paired = zip finalFS [0..]::[(Int,Int)]
    let prods = map mulpair paired
    let part1ans = sum prods
    print ("The answer to part 1 is " ++ show part1ans)
    return ()

testMovePart2 :: [(Int, Int)] -> (Int, Int) -> Maybe [(Int, Int)]
testMovePart2 inp val = do
    let checkFits v a = do
        let (id, run) = a
        let (_, run2) = v
        (id == (-1)) && (run2 <= run)
    let (inpa, inpb) = break (checkFits val) inp
    case inpb of
        [] -> Nothing -- didn't fit
        _ -> do
            let remaining = (snd $ head inpb) - (snd val)
            if remaining > 0 then Just (inpa ++ val : (-1, remaining) : (tail inpb))
            else Just (inpa ++ val : (tail inpb))

oneStepPart2 :: [(Int,Int)] -> [(Int, Int)] -> [(Int,Int)]
oneStepPart2 inp stay = do
    case inp of
        [] -> stay
        _ -> do
        let end = last inp
        let initinp = init inp
        case end of
            (-1,_) -> oneStepPart2 initinp (end:stay)
            _ -> do
                let newMoved = testMovePart2 initinp end
                case newMoved of
                    Nothing -> oneStepPart2 initinp (end:stay)
                    Just x -> oneStepPart2 x ((-1, snd end):stay)

flattenFS :: [(Int, Int)] -> [Int]
flattenFS inp = do
    let (val, run) = head inp
    let actval = if val < 0 then 0 else val
    let t = tail inp
    case t of
        [] -> replicate run actval
        _ -> replicate run actval ++ flattenFS t

getPart2Ans :: IO ()
getPart2Ans = do
    inp <- loadData "data/input.txt"
    let startingFS = convertDataToRun inp 0
    print startingFS
    let stay = oneStepPart2 startingFS []
    print stay
    let flattened = flattenFS stay
    print flattened
    let paired = zip flattened [0..]::[(Int,Int)]
    let prods = map mulpair paired
    let part2ans = sum prods
    print ("The answer to part 2 is " ++ show part2ans)
    return ()

