module Lib
    ( someFunc
    ) where

import Data.Array (Array)
import qualified Data.Array as A
import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as S

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

doStepHorizOld :: WorldState -> (Int, Int) -> WorldState
doStepHorizOld st dxdy = do
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

--Instead of accumulating modifications, keep track of all moving cells. Then use this together with move direction to figure out which 
-- . to insert and where to copy stuff from

getModificationsFromMovingSetAndRobotAndDir :: BoardState -> Set RobotLoc -> (Int, Int) -> RobotLoc -> [(RobotLoc, Char)]
getModificationsFromMovingSetAndRobotAndDir arr movingCells (dx,dy) (rx,ry) = do
    let adj (i,j) = (i-dx, j-dy)
    let adjNotIn x = not $ S.member (adj x) movingCells
    let trailingEdgeCells = S.filter adjNotIn movingCells
    let adjNotRobot (x,y)= x /= rx || y /= ry
    let nonRobotTrailingCells = S.filter adjNotRobot trailingEdgeCells
    let nonTrailingEdgeCells = S.toList $ S.difference movingCells nonRobotTrailingCells
    --let trailingMoves = trace ("Have trailing edge cells of " ++ show trailingEdgeCells ++ " and nonRobot trailing cells of " ++ show nonRobotTrailingCells) concatMap makeMove nonRobotTrailingCells 
    let makeMoveHole (x,y) = [((x,y),'.'), ((x+dx, y+dy), arr A.! (x,y))]
    let trailingMoves = trace ("Have trailing edge cells of " ++ show trailingEdgeCells ++ " and nonRobot trailing cells of " ++ show nonRobotTrailingCells) concatMap makeMoveHole nonRobotTrailingCells 
    let makeMove (x,y) = ((x+dx, y+dy), arr A.! (x,y))
    let internalMoves = trace ("Have internal cells of " ++ show nonTrailingEdgeCells) map makeMove nonTrailingEdgeCells
    trace ("Trailing moves : " ++ show trailingMoves ++ "  Internal moves : " ++ show internalMoves) trailingMoves ++ internalMoves

doStepHoriz :: WorldState -> (Int, Int) -> WorldState
doStepHoriz st dxdy = do
    let (arr, rob) = st
    let (x,y) = rob
    let (dx, dy) = dxdy
    let next_rob = (x+dx, y+dy)
    let (ableToShoveRest, moved_cells) = testStepHoriz (arr, rob) dxdy
    let modifications = trace ("The cells that will shift are " ++ show moved_cells) getModificationsFromMovingSetAndRobotAndDir arr moved_cells dxdy next_rob
    if ableToShoveRest then trace ("Robot able to shove crates! Modifications are "++ show modifications) (arr A.// ([(next_rob, '@'), (rob, '.')]++modifications), next_rob)
    else trace ("Robot stays at " ++ show rob) st  -- didn't find a . so nobody moves
--    case arr A.! next_rob of
--        '.' -> trace ("Robot moves to empty at "++show next_rob) (arr A.// [(next_rob, '@'), (rob, '.')], next_rob)
--        '#' -> trace ("Robot stuck") st
--        _ -> do
--            let (ableToShoveRest, moved_cells) = testStepHoriz (arr, next_rob) dxdy
--            let modifications = trace ("The cells that will shift are " ++ show moved_cells) getModificationsFromMovingSetAndRobotAndDir arr moved_cells dxdy rob
--            if ableToShoveRest then trace ("Robot able to shove crates!") (arr A.// ([(next_rob, '@'), (rob, '.')]++modifications), next_rob)
--                else trace ("Robot stays at " ++ show rob) st  -- didn't find a . so nobody moves


testStepHoriz:: WorldState -> (Int, Int) -> (Bool, Set RobotLoc)
testStepHoriz st dxdy = do
    let (arr, pos) = st
    let (x,y) = pos
    let (dx,dy) = dxdy
    let next_pos = (x+dx, y+dy)
    case arr A.! next_pos of
        '#' -> (False, S.empty)
        '.' -> (True, S.empty) --S.singleton next_pos)--[(next_pos, arr A.! pos)])
        _ -> (ableToShoveRest, S.insert next_pos modifications) where --(next_pos, arr A.! pos):modifications) where
                 (ableToShoveRest, modifications) = testStepHoriz (arr, next_pos) dxdy

doStepVertOld :: WorldState -> (Int, Int) -> WorldState
doStepVertOld st dxdy = do
    let (arr, rob) = st
    let (x,y) = rob
    let (dx, dy) = dxdy
    let next_rob = (x+dx, y+dy)
    case arr A.! next_rob of
        '.' -> trace ("Robot moves to empty at " ++ show next_rob) (arr A.// [(next_rob, arr A.! rob), (rob, '.')], next_rob) 
        'O' -> case findTarget st dxdy 1 of
            Nothing -> trace ("Stuck") st
            Just (sc, tc) -> trace ("Pushed normal block(s)") (arr A.// generateArrayManipCommand arr (sc, tc), next_rob)
        '[' -> trace ("Robot pushes on the left side of a crate and recruits the right side to push too") (arr, rob)
        ']' -> trace ("Robot pushes on the right side of a crate and recruits the left side to push too") (arr, rob)
        _ -> trace ("Stuck") (arr, rob)

doStepVert :: WorldState -> (Int, Int) -> WorldState
doStepVert st dxdy = do
    let (arr, rob) = st
    let (x,y) = rob
    let (dx, dy) = dxdy
    let next_rob = (x+dx, y+dy)
    let (ableToShoveRest, moved_cells) = testStepVert (arr, rob) dxdy False
    let modifications = trace ("The cells that will shift are " ++ show moved_cells) getModificationsFromMovingSetAndRobotAndDir arr moved_cells dxdy next_rob
    if ableToShoveRest then trace ("Robot able to shove crates! Modifications are "++ show modifications) (arr A.// ([(next_rob, '@'), (rob, '.')]++modifications), next_rob)
    else trace ("Robot stays at " ++ show rob) st  -- didn't find a . so nobody moves
    
--    let (ableToPush, modifications) = testStepVert (arr, next_rob) dxdy False False -- We don't recruit the direct vertical node from robot
--    if ableToPush then  trace ("Able to move crates. Doing modifications "++ show modifications ++ " and setting @ in " ++ show next_rob ++ " and setting . in " ++ show rob) (arr A.// ([(next_rob, '@'), (rob, '.')]++modifications), next_rob)
--    else trace ("Unable to move due to ableToPush at top level") st
--    case arr A.! next_rob of
--        '.' -> trace ("Robot moves to empty at "++show next_rob) (arr A.// [(next_rob, '@'), (rob, '.')], next_rob)
--        '#' -> trace ("Robot stuck") st
--        _ -> do
--            let (ableToShoveRest, modifications) = testStepVert (arr, next_rob) dxdy
--            if ableToShoveRest then trace ("Robot able to shove crates!") (arr A.// ([(next_rob, '@'), (rob, '.')]++modifications), next_rob)
--                else trace ("Robot stays at " ++ show rob) st  -- didn't find a . so nobody moves
--        '[' -> do
--            let (ableToShoveRestCol, modificationsCol) = testStepVert(arr, next_rob) dxdy
--            let rightCell = (x+dx, y+dy+1)
--            let (ableToShoveRestRight, modificationsRight) = testStepVert(arr, rightCell) dxdy
--            if ableToShoveRestCol && ableToShoveRestRight then trace ("Robot is able to shove the stack!") (arr A.// ([(next_rob, '@'), (rob, '.'), (rightCell, '.')]++modificationsCol++modificationsRight), next_rob)
--            else trace ("Can't shove this col!") st
--        ']' -> do
--            let (ableToShoveRestCol, modificationsCol) = testStepVert(arr, next_rob) dxdy
--            let leftCell = (x+dx, y+dy-1)
--            let (ableToShoveRestLeft, modificationsLeft) = testStepVert(arr, leftCell) dxdy
--            if ableToShoveRestCol && ableToShoveRestLeft then trace ("Robot is able to shove the stack!") (arr A.// ([(next_rob, '@'), (rob, '.'), (leftCell,'.')]++modificationsCol++modificationsLeft), next_rob)
--            else trace ("Can't shove this col!") st
--        _ -> trace ("Im hitting something unknown!") st

testStepVert:: WorldState -> (Int, Int) -> Bool -> (Bool, Set RobotLoc)
testStepVert st dxdy recruit = do
    let (arr, pos) = st
    let (x,y) = pos
    let (dx,dy) = trace ("Checking testStepVert at cell "++ show pos ++ " which is " ++ show (arr A.! pos)) dxdy
    let next_pos = (x+dx, y+dy)
    case arr A.! next_pos of
        '#' -> trace ("There is a wall") (False, S.empty)
        '.' -> trace ("There is a gap!") (True, S.empty)--[(next_pos, arr A.! pos)])
        'O' -> trace ("On an O, checking col") (ableToShoveRest, S.insert next_pos modifications) where
                    (ableToShoveRest, modifications) = testStepVert (arr, next_pos) dxdy False
        '[' -> do
                let next_pos_right = (x+dx, y+dy+1)
                let pos_right = (x,y+1)
                let (ableToShoveCol, modificationsCol) = testStepVert (arr, next_pos) dxdy False
                let (ableToShoveRight, modificationsRight) = testStepVert (arr, pos_right) dxdy True
                if recruit then (ableToShoveCol , S.insert next_pos  modificationsCol)
                else (ableToShoveCol && ableToShoveRight, S.insert next_pos (S.union modificationsCol modificationsRight))
        ']' -> do
                let next_pos_left = (x+dx, y+dy-1)
                let pos_left = (x,y-1)
                let (ableToShoveCol, modificationsCol) = testStepVert (arr, next_pos) dxdy False
                let (ableToShoveLeft, modificationsLeft) = testStepVert (arr, pos_left) dxdy True
                if recruit then (ableToShoveCol , S.insert next_pos modificationsCol)
                else (ableToShoveCol && ableToShoveLeft, S.insert next_pos (S.union modificationsCol modificationsLeft))
        _ -> (ableToShoveRest, S.insert next_pos modifications) where --(next_pos, arr A.! pos):modifications) where
                 (ableToShoveRest, modifications) = testStepVert (arr, next_pos) dxdy False


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

getPart1Ans :: String -> IO (Int)
getPart1Ans filename = do
    (bs, instrs, rl) <- loadData filename
    let (arr, _) = runPart1 (bs, rl) (concat instrs)
    return (gpsArr arr)

getPart2Ans :: String -> IO (Int)
getPart2Ans filename = do
    (bs, instrs, rl) <- loadDataPart2 filename
    let (arr, _) = runPart1 (bs, rl) (concat instrs)
    prettyPrint arr
    return (gpsArrPart2 arr)


prettyPrint :: BoardState -> IO ()
prettyPrint arr = do
    let elems = A.elems arr
    let bds = A.bounds arr
    let ((_,x0),(_,xf)) = bds
    let width = xf - x0 + 1
    let strs = chunksOf width elems 
    putStr $ unlines strs
    return ()

userTestLoop :: String -> IO ()
userTestLoop filename = do
    (bs, _, rl) <- loadData filename
    userTestHelper (bs,rl)

userTestLoop2 :: String -> IO ()
userTestLoop2 filename = do
    (bs, _, rl) <- loadDataPart2 filename
    userTestHelper (bs,rl)
    


userTestHelper :: WorldState -> IO ()
userTestHelper (bs,rl) = do
    prettyPrint bs
    print "Enter wasd motion command, c to cancel out:>"
    inp <- getChar
    case inp of
        'a' -> userTestHelper (doInstr (bs,rl) '<')
        's' -> userTestHelper (doInstr (bs,rl) 'v')
        'd' -> userTestHelper (doInstr (bs,rl) '>')
        'w' -> userTestHelper (doInstr (bs,rl) '^')
        'c' -> return ()
        _ -> userTestHelper (bs,rl)


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