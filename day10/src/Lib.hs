module Lib
    ( someFunc
    ) where
import Data.Set (Set)
import qualified Data.Set
import Debug.Trace
import Distribution.Simple.CCompiler (filenameCDialect)
someFunc :: IO ()
someFunc = putStrLn "someFunc"

getAdjacentInds :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
getAdjacentInds ind lim = do
    let (xi, yi) = ind
    let (xlim, ylim) = lim
    [(xi+x,yi+y)|x <- [-1, 0, 1], y <- [-1, 0, 1], x+xi >= 0, x+xi < xlim, y+yi >=0, y+yi < ylim, x /=0 || y /=0, x==0 || y==0]

steppable :: Int -> Int -> Bool
steppable a b = d == 1 where d = b-a

getInd ::[[Int]] -> Int -> Int -> Int
getInd arr x y =
    (arr!!y)!!x

getIndTuple :: [[Int]] -> (Int, Int) -> Int
getIndTuple arr ind =
    (arr!!y)!!x where (x,y) = ind

getEdges :: [[Int]] -> [((Int, Int), (Int, Int))]
getEdges inp = do
    let cols = length inp
    let rows = length $ head inp
    let inpInds = [(x,y) | x <- [0..cols-1], y <- [0..rows-1]]
    let pairInds = [(a, b) | a <- inpInds, b <- getAdjacentInds a (rows,cols)]
    let checkSteppable arr a = do
        let (ind1, ind2) = a
        let val1 = getIndTuple arr ind1
        let val2 = getIndTuple arr ind2
        steppable val1 val2

    trace (show pairInds) filter (checkSteppable inp) pairInds
    


loadData :: String -> IO [[Int]]
loadData filename = do
    fileStrs <- readFile filename
    let fileLines = lines fileStrs
    let readInt = read . (:"") 
    let myArray = map (map readInt) fileLines
    print myArray
    return (myArray)
    
getTrailheads ::[[Int]] -> [(Int, Int)]
getTrailheads arr = do
    let cols = length arr
    let rows = length $ head arr
    let inpInds = [(x,y) | x <- [0..cols-1], y <- [0..rows-1]]
    filter (\x -> getIndTuple arr x == 0) inpInds

getAdjacentNodes :: [[Int]] -> (Int, Int) -> [(Int, Int)]
getAdjacentNodes arr node = do
    let lim = (length arr, length (head arr))
    let potential = getAdjacentInds node lim
    trace ("Potential adjacents " ++ show potential ++ " for " ++ show node) filter (steppable (getIndTuple arr node) . getIndTuple arr) potential


getReachable9s :: [[Int]] -> Set (Int, Int) -> (Int,Int) -> Set (Int, Int)
getReachable9s arr closed trailhead = do
    let th_val = getIndTuple arr trailhead 
    if th_val == 9 then trace "At end point " Data.Set.singleton trailhead
    else  do
        let adjnodes = trace ("Not at end, at " ++ show th_val ++ show trailhead) getAdjacentNodes arr trailhead
        let openadjnodes = trace ("Adj nodes is " ++ show adjnodes) filter (\x -> not $ Data.Set.member x closed ) adjnodes
        trace ("Open adj nodes is " ++ show openadjnodes) foldl Data.Set.union Data.Set.empty (map (\x -> getReachable9s arr (Data.Set.insert x closed) x) openadjnodes)

countPathsTo9 :: [[Int]] -> Set (Int, Int) -> (Int,Int) -> Int
countPathsTo9 arr closed trailhead = do
    let th_val = getIndTuple arr trailhead 
    if th_val == 9 then trace "At end point " 1
    else  do
        let adjnodes = trace ("Not at end, at " ++ show th_val ++ show trailhead) getAdjacentNodes arr trailhead
        let openadjnodes = trace ("Adj nodes is " ++ show adjnodes) filter (\x -> not $ Data.Set.member x closed ) adjnodes
        trace ("Open adj nodes is " ++ show openadjnodes) sum (map (\x -> countPathsTo9 arr (Data.Set.insert x closed) x) openadjnodes)
    
doPart1 :: String -> IO Int
doPart1 filename = do
    myArray <- loadData filename
    let myEdges = getEdges myArray
    print (show myEdges)
    let trailheads = getTrailheads myArray
    print ("My trailheads are " ++ show trailheads)
    return (sum (map (Data.Set.size . getReachable9s myArray Data.Set.empty) trailheads))

doPart2 :: String -> IO Int
doPart2 filename = do
    myArray <- loadData filename
    let myEdges = getEdges myArray
    print (show myEdges)
    let trailheads = getTrailheads myArray
    print ("My trailheads are " ++ show trailheads)
    return (sum (map (countPathsTo9 myArray Data.Set.empty) trailheads))

testPart1 :: IO Int
testPart1 = do doPart1 "data/test1.txt"

runPart1 :: IO Int
runPart1 = do doPart1 "data/input.txt"

testPart2 ::IO Int
testPart2 = do doPart2 "data/test1.txt"

runPart2 :: IO Int
runPart2 = do doPart2 "data/input.txt"
