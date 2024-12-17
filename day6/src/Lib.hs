module Lib
    ( someFunc,
    getPart1Ans,
    getPart2Ans
    ) where
import Data.List.Split
import Data.List
import Data.Bits
import Data.Int
import Debug.Trace
import Control.Parallel.Strategies
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
someFunc :: IO ()
someFunc = putStrLn "someFunc"

loadData :: String -> IO [[Char]]
loadData filename = do
    inputText <- readFile filename
    let inputLines = lines inputText
    return inputLines

-- Changing direction mask to be 0 is none, 1 is up, 2 is left, 4 is down, 8 is right
findGuardInLine :: [Char] -> Maybe (Int, Int8) -- col x, direction (0 up, 1 right, 2 down, 3 left), and may not be in line
findGuardInLine arr_row = do
    let indexUp = elemIndex '^' arr_row
    let indexRight = elemIndex '>' arr_row
    let indexDown = elemIndex 'v' arr_row
    let indexLeft = elemIndex '<' arr_row
    case (indexUp, indexRight, indexDown, indexLeft) of
        (Just x, _, _, _) -> Just (x, 1)
        (_, Just x, _, _) -> Just (x, 2)
        (_, _, Just x, _) -> Just (x, 4)
        (_, _, _, Just x) -> Just (x, 8)
        (_, _, _, _) -> Nothing

elemIndexNotNothing :: [Maybe(Int, Int8)] -> Int -> Maybe (Int,Int,Int8)
elemIndexNotNothing matches row_ind = do
    if row_ind >= length matches then Nothing 
    else case (matches!!row_ind) of
        Nothing -> elemIndexNotNothing matches (row_ind + 1)
        Just (x, d) -> Just(x, row_ind, d)

findGuard :: [[Char]] -> Maybe (Int, Int, Int8) --col x , row y, direction (0 up, 1 right, 2 down, 3 left)
findGuard arr = do
    let guardInRow = map findGuardInLine arr
    let guardInCol = elemIndexNotNothing guardInRow 0
    guardInCol

markVal :: [[a]] -> (Int, Int) -> a -> [[a]]
markVal arr pos chr = do
    let x = fst pos
    let y = snd pos
    let (arr_bef, arr_after) = splitAt y arr
    let row_to_change = head arr_after
    let (row_bef, row_after) = splitAt x row_to_change
    let changed_row = row_bef ++ [chr] ++ (tail row_after)
    arr_bef ++ [changed_row] ++ (tail arr_after)

isObst :: Char -> Bool
isObst chr = case chr of
    '#' -> True
    'O' -> True
    _ -> False

getMotion :: Int8 -> (Int, Int)
getMotion dir = case dir of
    1 -> (0, (-1))
    2 -> (1, 0)
    4 -> (0, 1)
    8 -> ((-1), 0)
    _ -> (0, 0)

getTurnRight :: Int8 -> Int8
getTurnRight dir = case dir of
    1 -> 2
    2 -> 4
    4 -> 8
    8 -> 1
    _ -> 0

checkValid :: [[Char]] -> (Int, Int) -> Bool
checkValid arr coord = do
    let (x, y) = coord
    y < length arr && y >= 0 && x < length (arr!!0) && x >= 0


moveOnGrid :: [[Char]] -> (Int, Int, Int8) -> (Maybe (Int, Int, Int8), Bool)
moveOnGrid arr pos = do
    let (x, y, d) = pos
    let (dx, dy) = getMotion d
    if not $ checkValid arr (x+dx, y+dy) then (Nothing, False)
    else if not $ isObst (arr!!(y+dy)!!(x+dx)) then (Just (x+dx, y+dy, d), False)
    else do
        let turnedRightDir = getTurnRight d
        (Just (x, y, turnedRightDir), True)

getGuardChar :: Int8 -> Char
getGuardChar d = case d of
    0 -> 'O'
    1 -> '^'
    2 -> '>'
    4 -> 'v'
    8 -> '<'
    _ -> 'E'


moveGuard :: [[Char]] -> (Int, Int, Int8) -> ([[Char]], Maybe (Int, Int, Int8), Bool)
moveGuard arr pos = do
    let (x, y, _) = pos
    let removedGuardArr = markVal arr (x,y) 'X'
    let (newPos, hit) = moveOnGrid removedGuardArr pos
    case newPos of
        Nothing -> (removedGuardArr, Nothing, hit)
        Just (x_new,y_new,d_new) -> do
            let addedGuardChar = getGuardChar d_new
            let addedGuardArr = markVal removedGuardArr (x_new,y_new) addedGuardChar
            (addedGuardArr, newPos, hit)

showGrid ::[[Char]] -> IO [()]
showGrid arr = do
    sequence $ map (\x -> putStrLn x) arr

getBitmaskChar :: Int8 -> Char
getBitmaskChar val = case val of
    0 -> '0' -- No motion here
    1 -> '|' -- Motion up
    2 -> '-' -- Motion right
    3 -> '+' -- Motion up and right
    4 -> '|' -- Motion down
    5 -> '|' -- Motion up and down
    6 -> '+' -- Motion down and right
    7 -> '+' -- Motion down and right and up
    8 -> '-' -- Motion left
    9 -> '+' -- Motion up and left
    10 -> '-' -- Motion left and right
    11 -> '+' -- Motion left and right and up
    12 -> '+' -- Motion left and down
    13 -> '+' -- Motion left and up and down
    14 -> '+' -- Motion left and right and down
    15 -> '*' -- Motion in all directions
    _ -> '.'

showHist ::[[Int8]] -> IO [()]
showHist arr = do
    let charMap = map (map (getBitmaskChar)) arr
    sequence $ map print charMap

showHistRaw :: [[Int8]] -> IO [()]
showHistRaw arr = do
    let charMap = map (map (show)) arr
    sequence $ map print charMap

executePatrol :: [[Char]] -> Maybe (Int, Int, Int8) -> [[Char]]
executePatrol arr guardPos = do
    case guardPos of
        Nothing -> arr
        Just (x, y, d) -> do 
            let (newArr, newGuardPos, _) = moveGuard arr (x,y,d)
            executePatrol newArr newGuardPos

getSetIndex :: Int -> Int -> Int -> Int
getSetIndex arrlen x y = do
    x + arrlen * y

patrolLoopCheck :: [[Char]] -> Map Int Int8-> Maybe (Int, Int, Int8) -> Bool
patrolLoopCheck arr hist guardPos = do
    case guardPos of
        Nothing -> False  -- She is off the grid so this wasn't a loop
        Just (x, y, d) -> do -- She is still on the grid
            let (_, newGuardPos, hit) = moveGuard arr (x,y,d)
            if hit then do -- If we hit something, we need to do a loop check
                let setind = getSetIndex (length arr) x y
                let currHist = Map.lookup setind hist
                case currHist of
                    Nothing -> do
                        let newHist = Map.insert setind d hist
                        patrolLoopCheck arr newHist newGuardPos
                    Just x -> do
                        if (x .&. d /= 0) then True
                        else do
                            let newDirMask = x .|. d
                            let newHist = Map.insert setind newDirMask hist
                            patrolLoopCheck arr newHist newGuardPos
            else patrolLoopCheck arr hist newGuardPos

countXs :: [[Char]] -> Int
countXs arr = do
    sum (map (length . filter (=='X')) arr)

getPart1Ans :: String -> IO (Int)
getPart1Ans filename = do
    arr <- loadData filename
    let gp = findGuard arr
    let newArr = executePatrol arr gp
    showGrid newArr
    return (countXs newArr)

checkForLoopsWithObstacle :: [[Char]] -> Map Int Int8-> (Int, Int) -> Maybe(Int, Int, Int8) -> Bool
checkForLoopsWithObstacle arr hist obst gp = do
    let tmparr = markVal arr obst 'O'
    patrolLoopCheck tmparr hist gp

getPart2Ans :: String -> IO (Int)
getPart2Ans filename = do
    arr <- loadData filename
    let gp = findGuard arr
    let history = Map.empty
    let indsY = [0..((length arr)-1)]
    let indsX = [0..(((length (arr!!0)-1)))]

    let obstacleCausesLoop = parMap rdeepseq (\y -> map (\x -> trace ((show x)++","++(show y)) checkForLoopsWithObstacle arr history (x,y) gp) indsX) indsY
    let totalLoopPositions = sum $ map (length . filter (==True)) obstacleCausesLoop
    return totalLoopPositions

checkPart2 :: String -> (Int, Int) -> IO (Int)
checkPart2 filename loc = do
    arr <- loadData filename
    let gp = findGuard arr
    let history = Map.empty --initializeEmptyHistory arr
    if checkForLoopsWithObstacle arr history loc gp then do
        print ("There is a loop")
        return 1
    else do
        print ("There is no loop")
        return 0


