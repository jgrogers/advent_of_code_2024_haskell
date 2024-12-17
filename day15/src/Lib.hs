module Lib
    ( someFunc
    ) where

import Data.Array (Array)
import qualified Data.Array as A
import Data.List
import Data.List.Split

import Debug.Trace

type RobotLoc = (Int, Int)
type BoardState = Array RobotLoc Char
type WorldState = (BoardState, RobotLoc)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

parseData :: [String] -> (BoardState, [String], Maybe RobotLoc)
parseData strs = do
    let (a,b) = span (/= "") strs
    let rows = length a
    let cols = length (head a)
    let arr = A.array ((0,0), (rows-1, cols-1)) [((rowIndex, colIndex), value) | (rowIndex, row) <- zip [0..] a, (colIndex, value) <- zip [0..] row]
    let rob = filter (\i -> arr A.! i == '@') [(i,j)| i<-[0..(rows-1)], j<-[0..(cols-1)]]
    case rob of
        [] -> (arr, tail b, Nothing)
        _ -> (arr, tail b, Just (head rob))

tf :: Char -> String
tf a = case a of
    '#' -> "##"
    '@' -> "@."
    'O' -> "[]"
    '.' -> ".."
    _ -> [a,a]
expandPart2 ::String -> String
expandPart2 = concatMap tf 

parseDataPart2 :: [String] -> (BoardState, [String], Maybe RobotLoc)
parseDataPart2 strs = do
    let (a,b) = span (/= "") strs

    let expanded = map expandPart2 a
    let rows = length expanded
    let cols = length (head expanded)
    let arr = A.array ((0,0), (rows-1, cols-1)) [((rowIndex, colIndex), value) | (rowIndex, row) <- zip [0..] expanded, (colIndex, value) <- zip [0..] row]
    let rob = filter (\i -> arr A.! i == '@') [(i,j)| i<-[0..(rows-1)], j<-[0..(cols-1)]]
    case rob of
        [] -> (arr, tail b, Nothing)
        _ -> (arr, tail b, Just (head rob))


loadData :: String -> IO (BoardState, [String], RobotLoc)
loadData filename = do
    fileStr <- readFile filename
    let fileLines = lines fileStr
    let (arr, instrs, rob) = parseData fileLines
    case rob of
        Nothing -> error "Didn't find robot!"
        Just x -> return (arr, instrs, x)

loadDataPart2 :: String -> IO (BoardState, [String], RobotLoc)
loadDataPart2 filename = do
    fileStr <- readFile filename
    let fileLines = lines fileStr
    let (arr, instrs, rob) = parseDataPart2 fileLines
    case rob of
        Nothing -> error "Didn't find robot!"
        Just x -> return (arr, instrs, x)

findTarget :: WorldState -> (Int, Int) -> Int -> Maybe (RobotLoc, RobotLoc)
findTarget (arr, start) delta num = do
    let (x,y) = start
    let (dx, dy) = delta
    let candidate = (x + num*dx, y + num*dy)
    if A.inRange (A.bounds arr) candidate then do
        case arr A.! candidate of
            '#' -> Nothing
            '.' -> Just (start, candidate)
            _ -> findTarget (arr, start) delta (num+1)
    else Nothing

generateArrayManipCommand:: BoardState -> (RobotLoc, RobotLoc) -> [(RobotLoc, Char)]
generateArrayManipCommand arr (st, ed) = do
    let (xs, ys) = st
    let (xe, ye) = ed
    let d e s | e > s = 1
              | e < s = (-1)
              | otherwise = 0
    let dx = d xe xs
    let dy = d ye ys
    case (dx, dy) of
        (0 ,-1) -> (st, arr A.! ed):[((xs, y+dy), arr A.!(xs, y)) | y <- [(min ys ye+1)..(max ys ye)]]
        (0 , 1) -> (st, arr A.! ed):[((xs, y+dy), arr A.!(xs, y)) | y <- [(min ys ye)..(max ys ye-1)]]
        (-1, 0) -> trace ("Moving in the -dx direction ") (st, arr A.! ed):[((x+dx, ys), arr A.!(x, ys)) | x <- [(min xs xe+1)..(max xs xe)]]
        (1, 0) -> trace ("Moving in the +dx direction ") (st, arr A.! ed):[((x+dx, ys), arr A.!(x, ys)) | x <- [(min xs xe)..(max xs xe-1)]]
        (_,_) -> trace ("No motion I guess...") []

doStepHoriz :: WorldState -> (Int, Int) -> WorldState
doStepHoriz st dxdy = do
    let (arr, rob) = st
    let (x,y) = rob
    let (dx, dy) = dxdy
    let next_rob = (x+dx, y+dy)
    if arr A.! next_rob == '.' then trace ("Robot moves to empty at "++show next_rob) (arr A.// [(next_rob, '@'), (rob, '.')], next_rob)
    else do
        case findTarget st dxdy 1 of
            Nothing -> trace ("Robot stays at " ++ show rob) st  -- didn't find a . so nobody moves
            --Just (sc, tc) -> (arr A.// [(next_rob, '@'), (rob, '.'), (tc, 'O')], next_rob)
            Just (sc, tc) -> trace ("Robot pushes a box and moves to " ++ show next_rob) (arr A.// generateArrayManipCommand arr (sc, tc), next_rob)

vertStepHelper :: BoardState -> (Int, Int)

doStepVert :: WorldState -> (Int, Int) -> WorldState
doStepVert st dxdy = do
    let (arr, rob) = st
    let (x,y) = rob
    let (dx, dy) = dxdy
    let next_rob = (x+dx, y+dy)
    case arr A.! next_rob of
        '.' -> (arr A.// [(next_rob, arr A.! rob), (rob, '.')], next_rob) 
    if arr A.! next_rob == '.' then 
    else do
        case arr A.! next_rob of
            '['
        case findTarget st dxdy 1 of
            Nothing -> trace ("Robot stays at " ++ show rob) st  -- didn't find a . so nobody moves
            --Just (sc, tc) -> (arr A.// [(next_rob, '@'), (rob, '.'), (tc, 'O')], next_rob)
            Just (sc, tc) -> trace ("Robot pushes a box and moves to " ++ show next_rob) (arr A.// generateArrayManipCommand arr (sc, tc), next_rob)

slideRow :: WorldState -> Bool -> WorldState
slideRow st dir = do
    let (arr, rob) = st
    let (y,x) = rob
    let (dy, dx) = (0, 1)
    undefined

doInstr :: WorldState -> Char -> WorldState
doInstr st instr = do
    case instr of
        '<' -> doStepHoriz st (0, -1)
        'v' -> doStepVert st (1, 0)
        '>' -> doStepHoriz st (0, 1)
        '^' -> doStepVert st (-1, 0)
        _ -> st

runPart1 :: WorldState -> String -> WorldState
runPart1 = foldl' doInstr

prettyPrint :: BoardState -> IO ()
prettyPrint arr = do
    let elems = A.elems arr
    let bds = A.bounds arr
    let ((_,x0),(_,xf)) = bds
    let width = xf - x0 + 1
    let strs = chunksOf width elems 
    putStr $ unlines strs
    return ()

gpsxy :: RobotLoc -> Int
gpsxy (y,x) = x + 100 * y

gpsArr :: BoardState -> Int
gpsArr arr = do
    let boxes = filter (\i -> arr A.! i == 'O') (A.indices arr)
    sum $ map gpsxy boxes

gpsArrPart2 :: BoardState -> Int
gpsArrPart2 arr = do
    let boxes = filter (\i -> arr A.! i == '[') (A.indices arr)
    sum $ map gpsxy boxes