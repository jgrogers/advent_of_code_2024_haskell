module Lib
    ( someFunc
    ) where

import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as S
import Data.Array (Array, (!))
import qualified Data.Array as A
import Debug.Trace
import qualified Data.PQueue.Prio.Min as MinQ

import Control.Monad (forM, when)
import Control.Monad.State (State, get, put, execState, StateT, execStateT, runStateT, lift)


type MazeCell = (Int, Int)
type MazeState = Array MazeCell Char
type MazeProblem = (MazeState, MazeCell, MazeCell)
type StateCost = (MazeCell, Integer)
type SearchState = Array MazeCell Integer -- Cost to come to each search state
type MazePQ = MinQ.MinPQueue Integer MazeCell
someFunc :: IO ()
someFunc = putStrLn "someFunc"

getActions :: MazeState -> MazeCell -> Integer -> [StateCost] -- get the legal actions and incremental costs
getActions arr curr_state ctc = do
    let (x,y) = curr_state
    let ((x0,y0),(x1,y1)) = A.bounds arr
    let possible_actions = [(x+1,y),(x, y+1),(x-1, y), (x,y-1)]
    let in_maze_actions = filter inMaze possible_actions where inMaze (i,j) = i>=x0 && i <= x1 && j >= y0 && j <= y1
    let actions_that_dont_hit = filter legal in_maze_actions where legal (i,j) = arr!(i,j) /= '#'
    zip actions_that_dont_hit (repeat (1+ctc))

enqueuePQ :: StateCost -> MazePQ -> MazePQ
enqueuePQ sc pq = do
    let (mcd, cost) = sc
    MinQ.insert cost mcd pq

brushfireMaze' :: MazeProblem -> MazePQ -> Maybe Integer -> StateT SearchState IO (Maybe Integer)
brushfireMaze' mp open_queue best_so_far = do
    cost_to_come <- get
    let (ms, _, goal) = mp
    if MinQ.null open_queue then return best_so_far
    else do
        let ((ctc, next_state), remaining_queue) = MinQ.deleteFindMin open_queue
        let (x,y) = next_state
        let (gx, gy) = goal
        let curr_ctc = cost_to_come ! next_state
        if curr_ctc == -1 || curr_ctc > ctc then do
            let new_cost_to_come = cost_to_come A.// [(next_state, ctc)]
            put new_cost_to_come
            case best_so_far of
                Nothing -> do
                    if x == gx && y == gy then brushfireMaze' mp remaining_queue (Just ctc) --return (Just ctc) --brushfireMaze' mp remaining_queue (Just ctc)
                    else
                        let actions = getActions ms next_state ctc
                            next_open_queue = Data.List.foldr enqueuePQ remaining_queue actions
                        in brushfireMaze' mp next_open_queue best_so_far
                Just best_ctc -> do
                    if x == gx && y == gy then
                        if ctc > best_ctc then brushfireMaze' mp remaining_queue (Just ctc) -- return best_so_far --Remaining paths are strictly suboptimal
                        else brushfireMaze' mp remaining_queue best_so_far
                    else
                        let actions = getActions ms next_state ctc
                            next_open_queue = Data.List.foldr enqueuePQ remaining_queue actions
                        in brushfireMaze' mp next_open_queue best_so_far
        else brushfireMaze' mp remaining_queue best_so_far
    
brushfireMaze :: MazeProblem -> IO (Maybe Integer)
brushfireMaze mp = do
    let (arr, s, e) = mp
    let ((i0, j0),(i1,j1)) = A.bounds arr
    let cost_to_come = A.array ((i0, j0), (i1, j1)) [((i,j), -1)| i <-[i0..i1], j<-[j0..j1]]
    let (i,j) = s
    finalMP <- runStateT (brushfireMaze' mp (MinQ.singleton 0 (i,j)) Nothing) cost_to_come
    return (fst finalMP)

brushfireCheat :: MazeProblem -> [(Int, Int)] -> IO (Maybe Integer)
brushfireCheat mp cheats = do
    let (arr, s, e) = mp
    let ((i0, j0),(i1,j1)) = A.bounds arr
    let cost_to_come = A.array ((i0, j0), (i1, j1)) [((i,j), -1)| i <-[i0..i1], j<-[j0..j1]]
    let (i,j) = s
    let arr_cheated = arr A.// [((a,b),'.')|(a,b)<-cheats]
    let mp = (arr_cheated, s, e)
    finalMP <- runStateT (brushfireMaze' mp (MinQ.singleton 0 (i,j)) Nothing) cost_to_come
    return (fst finalMP)

getCTC :: MazeProblem -> IO (Array (Int, Int) Integer)
getCTC mp = do
    let (arr, s, e) = mp
    let ((i0, j0),(i1,j1)) = A.bounds arr
    let cost_to_come = A.array ((i0, j0), (i1, j1)) [((i,j), -1)| i <-[i0..i1], j<-[j0..j1]]
    let (i,j) = s
    finalMP <- runStateT (brushfireMaze' mp (MinQ.singleton 0 (i,j)) Nothing) cost_to_come
    return (snd finalMP)

getCheatValue :: Array (Int, Int) Integer -> ((Int, Int), (Int, Int)) -> Integer
getCheatValue arr cheat = val where
    val = old_ctc - new_ctc
    old_ctc = arr A.! end
    new_ctc = arr A.! start + getCheatSteps cheat
    (start, end) = cheat

getCheatSteps :: ((Int, Int), (Int, Int)) -> Integer
getCheatSteps ((si,sj),(ei,ej)) = fromIntegral (abs (si - ei) + abs (sj - ej))

checkCheatSkipsOverOnlyWalls :: Array (Int, Int) Char -> ((Int, Int), (Int, Int)) -> Bool
checkCheatSkipsOverOnlyWalls arr cheat =
    let (start, end) = cheat
        (si, sj) = start
        (ei, ej) = end
        cells = if si == ei then [(si,j)| j <-[(min sj ej)..(max sj ej)], j /= sj, j/= ej]
                else [(i,sj)| i <-[(min si ei)..(max si ei)], i /= si, i/= ei]
    in trace ("Checking cheat " ++ show cheat ++ " cells for walls " ++ show cells ++ " which have these values in array " ++ show (map (\a -> arr A.! a) cells))foldr ((&&).(\c -> arr A.! c == '#')) True cells

doPart1 :: String -> Integer -> IO (Int)
doPart1 fname lim = do
    mp <- loadData fname
    ctc <- getCTC mp
    prettyPrintCTC ctc
    -- Now we can analyze the CTC matrix. Look for start and end positions that are legal cheats and count up the time savings
    -- Legal cheats are from start positions with populated ctc values
    -- Legal cheats end either 1 or 2 steps away, like S##E or S#E
    -- Search pattern
    -- S    E   S   E   S#E     E#S
    -- #    #   #   #   S##E    E##S
    -- #    #   E   S
    -- E    S
    -- The cost savings will be computed as : the old CTC on E minus (CTC on S + 2 or 3 depending on if we cheated for 1 or 2 steps)
    let (arr, _, _) = mp
    let ((i0, j0), (i1,j1)) = A.bounds ctc
    let cheat_starts = [(si,sj)|si <- [i0..i1], sj <- [j0..j1],ctc A.! (si,sj) /= -1]
    let cheat_end_offsets = [(0,2),(0,-2),(0,3),(0,-3),(2,0),(3,0),(-2,0),(-3,0)]
    let possible_cheats = [((si,sj),(si+off_i,sj+off_j)) | (si,sj)<-cheat_starts, (off_i, off_j) <- cheat_end_offsets]
    let legal_cheats = trace ("Possible cheats are : " ++ show possible_cheats) [((si, sj),(ei, ej))| ((si,sj),(ei,ej)) <- possible_cheats , ei >=i0, ei <= i1, ej >= j0, ej<=j1, ctc A.! (ei,ej) /= -1]
    let best_legal_cheats = trace("Had " ++ show (length legal_cheats) ++ " before making sure they only are on walls") filter (checkCheatSkipsOverOnlyWalls arr) legal_cheats
    let res = trace ("Legal cheats - have " ++ show (length best_legal_cheats) ++ " are : " ++ show best_legal_cheats) length $ filter (>=lim) (map (getCheatValue ctc) best_legal_cheats)
    return res

getAllUsefulEndsFromStart :: Array (Int, Int) Integer -> Array (Int, Int) Char -> Integer -> Integer -> (Int, Int) -> [(Int, Int)]
getAllUsefulEndsFromStart ctc arr cheatlen cheat_value_lim start = 
    trace ("Getting useful ends from " ++ show start) filter (\a -> getCheatValue ctc (start,a) >= cheat_value_lim) legal_ends where
        legal_ends = filter (\b -> getCheatSteps (start, b) <= cheatlen) possible_ends
        possible_ends = [(si,sj)|si <- [i0..i1], sj <- [j0..j1],arr A.! (si,sj) /= '#']
        ((i0,j0),(i1,j1)) = A.bounds arr

getPlanFromSolve :: Array (Int, Int) Integer -> (Int, Int) -> [(Int, Int)]
getPlanFromSolve ctc location =
    -- Find the adjacent location with a lower cost to come. Searching from end to start. only one of 4 connected will be lower than center
    let my_ctc = ctc A.! location
        (x,y) = location
        next_loc = [(x+1, y),(x-1, y), (x,y-1), (x,y+1)]
        ((i0,j0),(i1,j1)) = A.bounds ctc
        legal_locs = filter (\a-> let (i,j) = a in i>=i0 && j >= j0 && i <= i1 && j <= j1 && ctc A.! a /= -1 && ctc A.! a < my_ctc) next_loc 
    in case legal_locs of
        [] -> [location]
        [s] -> getPlanFromSolve ctc s ++ [location]
        _ -> error ("Branching paths found in reverse search -- might need to compute a single best from old part1")

doPart2 :: String -> Integer -> Integer -> IO (Int)
doPart2 fname lim cheatlen = do
    mp <- loadData fname
    ctc <- getCTC mp
    prettyPrintCTC ctc
    -- I think for part 2 we need to flip our thinking. What we can do is look at every S in the CTC /= -1 and find each E that meets the cheat value threshold
    -- then see if it is a legal cheat (manhattan distance <= cheatlen)
    let (arr, s, e) = mp
    let ((i0, j0), (i1,j1)) = A.bounds ctc
    --let cheat_starts = [(si,sj)|si <- [i0..i1], sj <- [j0..j1],arr A.! (si,sj) /= '#']
    let cheat_starts = getPlanFromSolve ctc e
    let useful_cheat_count_from_point = map (length . getAllUsefulEndsFromStart ctc arr cheatlen lim) cheat_starts
    return (sum useful_cheat_count_from_point)
    -- now we can't form cheat offsets bc that is stupid


parseData :: [String] -> (MazeState, Maybe MazeCell, Maybe MazeCell)
parseData strs = do
    let (a,b) = span (/= "") strs
    let rows = length a
    let cols = length (head a)
    let arr = A.array ((0,0), (rows-1, cols-1)) [((rowIndex, colIndex), value) | (rowIndex, row) <- zip [0..] a, (colIndex, value) <- zip [0..] row]
    let start = filter (\i -> arr A.! i == 'S') [(i,j)| i<-[0..(rows-1)], j<-[0..(cols-1)]]
    let end = filter (\i -> arr A.! i == 'E') [(i,j)| i<-[0..(rows-1)], j<-[0..(cols-1)]]
    case (start, end) of
        ([],[]) -> (arr, Nothing, Nothing)
        (_,[]) -> (arr, Just (head start), Nothing)
        ([],_) -> (arr, Nothing, Just (head end))
        (_,_) -> (arr, Just (head start), Just (head end))

loadData :: String -> IO MazeProblem
loadData fname = do
    fileStr <- readFile fname
    let fileLines = lines fileStr
    let (arr, start, end) = parseData fileLines
    case (start, end) of
        (Just s, Just e) -> return (arr, s, e)
        (_, _) -> error "Didn't find start or end!!"

prettyPrint :: MazeState -> IO ()
prettyPrint arr = do
    let elems = A.elems arr
    let bds = A.bounds arr
    let ((_,x0),(_,xf)) = bds
    let width = xf - x0 + 1
    let strs = chunksOf width elems 
    putStr $ unlines strs
    return ()

prettyPrintCTC :: Array (Int, Int) Integer -> IO ()
prettyPrintCTC arr = do
    let elems = A.elems arr
    let bds = A.bounds arr
    let ((_,x0),(_,xf)) = bds
    let width = xf - x0 + 1
    let costs = chunksOf width elems 
    let strs = map show costs
    putStr $ unlines strs
    return ()
