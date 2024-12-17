module Lib
    ( someFunc
    ) where
import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace

import Control.Monad (forM, when)
import Control.Monad.State (State, get, put, execState, StateT, execStateT, runStateT, lift)
import qualified Data.Array.IO as IA

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getNeighbors :: Array (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
getNeighbors arr ind = do
    let bds = A.bounds arr
    let ((bdsi0, bdsj0),(bdsi, bdsj)) = bds
    let (i,j) = ind
    [(i+dx,j+dy)| dx <- [-1,0,1], dy<- [-1,0,1], dx/= dy, i+dx >= 0, j+dy>=0, i+dx <= bdsi, j+dy <= bdsj, dx == 0 || dy == 0]

getBorders :: Array (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
getBorders arr ind = do
    let bds = A.bounds arr
    let ((bdsi0, bdsj0),(bdsi, bdsj)) = bds
    let (i,j) = ind
    [(i+dx,j+dy)| dx <- [-1,0,1], dy<- [-1,0,1], dx/= dy, i+dx < 0  || j+dy < 0 ||  i+dx > bdsi || j+dy > bdsj, dx == 0 || dy == 0]

countMatch :: Array (Int, Int) Char -> Char -> [(Int, Int)] -> Int
countMatch arr val inds = do
    length $ filter (\i -> arr!i == val) inds

countNotMatch :: Array (Int, Int) Char -> Char -> [(Int, Int)] -> Int
countNotMatch arr val inds = do
    length $ filter (\i -> arr!i /= val) inds

countBorders :: Array (Int, Int) Char -> (Int, Int) -> Int
countBorders arr ind = length $ getBorders arr ind

getPerimeterElem :: Array  (Int, Int) Char -> (Int, Int) -> Int
getPerimeterElem arr ind = countNotMatch arr (arr!ind) (getNeighbors arr ind) + countBorders arr ind

getElemMap :: Array (Int, Int) Char -> Map Char Int
getElemMap arr = do
    let f i =  M.insertWith (+) (arr!i) 1
    foldr f M.empty (A.indices arr)

inBoundsOfArray :: Array (Int, Int) Char -> (Int, Int) -> Bool
inBoundsOfArray arr ind = do
    let ((bdsi0, bdsj0),(bdsi, bdsj)) = A.bounds arr
    let (x,y) = ind
    let res = x>=bdsi0 && x <= bdsi && y >= bdsj0 && y <= bdsj
--    trace ("x y = " ++ show ind ++ " gets " ++ show res)  res
    res

getRegion' ::Array (Int, Int) Char -> (Int, Int) -> Char -> Set (Int, Int) -> Set (Int,Int)
getRegion' arr seed val closed = do
    if not (inBoundsOfArray arr seed ) || (arr!seed /= val) then closed
    else do
        let (x, y) = seed
        let newclosed = S.insert seed closed
        let over = getRegion' arr (x+1, y) val newclosed 
        let down = getRegion' arr (x, y+1) val newclosed
        S.union (S.singleton seed) (S.union over down)

getRegion :: Array (Int, Int) Char -> (Int, Int) -> [(Int,Int)]
getRegion arr seed = S.elems (getRegion' arr seed (arr!seed) S.empty)

getRegions :: Array (Int, Int) Char -> [(Int, Int)]
getRegions arr = undefined

getPerimMap arr = do
    let f i = M.insertWith (+) (arr!i) (getPerimeterElem arr i)
    foldr f M.empty (A.indices arr)
loadData :: String -> IO (Array (Int, Int) Char)
loadData filename = do
    fileStr <- readFile filename
    let fileLines = lines fileStr
    let height = length fileLines
    let width = length $ head fileLines
    let assoc = zip [(i,j)| i<-[0..(height-1)], j <- [0..(width-1)]] (concat fileLines)
    let arr = A.array ((0,0),(height-1, width-1)) assoc
    return arr

part1 :: String -> IO Int
part1 filename = do
    arr <- loadData filename
    let perimMap = getPerimMap arr
    let elemMap = getElemMap arr
    let f a = (perimMap M.! a) * (elemMap M.! a)
    let costs = map f (M.keys perimMap)
    print (show costs)
    return (sum costs)
    
filtFunc :: Array (Int, Int) Char -> Char -> Set(Int, Int) -> (Int,Int) -> Bool
filtFunc arr chr cs2 x = (arr!x) == chr && not (S.member x cs2)

findConnectedComponent' :: Array(Int, Int) Char -> (Int,Int) -> Char -> StateT (Set (Int, Int)) IO ()
findConnectedComponent' arr ind chr = do
    cs <- get
    let cs2 =S.insert ind cs
    put cs2
    let neighbors = getNeighbors arr ind
    let unsearchedNeighbors = filter (filtFunc arr chr cs2) neighbors 
    let cs3 = S.union cs2 (S.fromList unsearchedNeighbors)
    put cs3
    mapM_ (\s -> findConnectedComponent' arr s chr) unsearchedNeighbors
    
findConnectedComponent :: Array(Int, Int) Char -> (Int, Int) -> IO [(Int, Int)] 
findConnectedComponent arr ind = do
    let chr = arr!ind
    finalSet <- runStateT (findConnectedComponent' arr ind chr) S.empty
    return (S.elems (snd finalSet))

findConnectedComponents' :: Array(Int, Int) Char -> [(Int, Int)] -> [[(Int, Int)]] -> IO [[(Int, Int)]]
findConnectedComponents' arr inds partial = do
    case inds of
        [] -> return (partial)
        _ -> do
            let hs = head inds
            nextConnectedComp <- findConnectedComponent arr hs
            let next_inds = S.toList $ S.difference (S.fromList inds) (S.fromList nextConnectedComp)
            let newPartial = nextConnectedComp : partial
            findConnectedComponents' arr next_inds newPartial

findConnectedComponents :: Array(Int, Int) Char -> IO [[(Int, Int)]]
findConnectedComponents arr = do
    let inds = A.indices arr
    findConnectedComponents' arr inds []

    
getPerimeterOfComponent :: Array(Int, Int) Char -> [(Int, Int)] -> Int
getPerimeterOfComponent arr inds = do
    sum $ map (getPerimeterElem arr) inds

costOfFenceForComponent arr inds = do
    let perim_cost = getPerimeterOfComponent arr inds
    let area_cost = length inds
    perim_cost * area_cost

getPart1Ans :: IO Int
getPart1Ans = do
    arr <- loadData "data/input.txt"
    comps <- findConnectedComponents arr
    return (sum $ map (costOfFenceForComponent arr) comps)