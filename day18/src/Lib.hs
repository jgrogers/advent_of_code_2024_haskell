module Lib
    ( someFunc
    ) where
import Data.List.Split
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Array (Array, (!))
import qualified Data.Array as A
import Debug.Trace
import qualified Data.PQueue.Prio.Min as MinQ

import Control.Monad (forM, when)
import Control.Monad.State (State, get, put, execState, StateT, execStateT, runStateT, lift)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type MazeCell = (Int, Int)
type MazeState = Array MazeCell Char
type MazeProblem = (MazeState, MazeCell, MazeCell)
type StateCost = (MazeCell, Integer)
type SearchState = Array MazeCell Integer -- Cost to come to each search state
type MazePQ = MinQ.MinPQueue Integer MazeCell

getActions :: MazeState -> MazeCell -> Integer -> [StateCost] -- get the legal actions and incremental costs
getActions arr curr_state ctc = do
    let (x,y) = curr_state
    let ((x0,y0),(x1,y1)) = A.bounds arr
    let possible_actions = [(x+1,y),(x, y+1),(x-1, y), (x,y-1)]
    let in_maze_actions = filter inMaze possible_actions where inMaze (i,j) = i>=x0 && i <= x1 && j >= y0 && j <= y1
    let actions_that_dont_hit = filter legal in_maze_actions where legal (i,j) = arr!(i,j) /= '#'
    zip actions_that_dont_hit (repeat (1+ctc))

--getUnActions :: MazeState -> MazeCell -> Integer -> [StateCost] -- get the legal actions and incremental costs
--getUnActions ms curr_state ctc = do
--    let (x,y,r) = curr_state
--    let unrotation_actions_with_costs = getUnRotationActions (curr_state, ctc)
--    let ((next_x, next_y, next_r), fwd_ctc) = getReverseAction (curr_state, ctc)
--    if ms ! (next_x, next_y) /= '#' then ((next_x, next_y, next_r),fwd_ctc):unrotation_actions_with_costs
--    else unrotation_actions_with_costs

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
        case best_so_far of
            Nothing -> do
                if x == gx && y == gy then return (Just ctc) --brushfireMaze' mp remaining_queue (Just ctc)
                else do
                    let curr_ctc = cost_to_come ! next_state
                    if curr_ctc == -1 || curr_ctc > ctc then do
                        let new_cost_to_come = cost_to_come A.// [(next_state, ctc)]
                        put new_cost_to_come
                        let actions = getActions ms next_state ctc
                        let next_open_queue = Data.List.foldr enqueuePQ remaining_queue actions
                        brushfireMaze' mp next_open_queue best_so_far
                    else brushfireMaze' mp remaining_queue best_so_far
            Just best_ctc -> do
                if x == gx && y == gy then
                    if ctc > best_ctc then return best_so_far --Remaining paths are strictly suboptimal
                    else brushfireMaze' mp remaining_queue best_so_far
                else do
                    let curr_ctc = cost_to_come ! next_state
                    if curr_ctc == -1 || curr_ctc > ctc then do
                        let new_cost_to_come = cost_to_come A.// [(next_state, ctc)]
                        put new_cost_to_come
                        let actions = getActions ms next_state ctc
                        let next_open_queue = Data.List.foldr enqueuePQ remaining_queue actions
                        brushfireMaze' mp next_open_queue best_so_far
                    else brushfireMaze' mp remaining_queue best_so_far


    
brushfireMaze :: MazeProblem -> IO (Maybe Integer)
brushfireMaze mp = do
    let (arr, s, e) = mp
    let ((i0, j0),(i1,j1)) = A.bounds arr
    let cost_to_come = A.array ((i0, j0), (i1, j1)) [((i,j), -1)| i <-[i0..i1], j<-[j0..j1]]
    let (i,j) = s
    finalMP <- runStateT (brushfireMaze' mp (MinQ.singleton 0 (i,j)) Nothing) cost_to_come
    return (fst finalMP)

--countOptimalCellsMaze :: MazeProblem -> IO (Int)
--countOptimalCellsMaze mp = do
--    let (arr, s, e) = mp
--    let ((i0, j0),(i1,j1)) = A.bounds arr
--    let cost_to_come = A.array ((i0, j0, 0), (i1, j1, 3)) [((i,j, k), -1)| i <-[i0..i1], j<-[j0..j1], k<-[0..3]]
--    let (i,j) = s
--    let (ie, je) = e
--    finalMP <- runStateT (brushfireMaze' mp (MinQ.singleton 0 (i,j,0)) Nothing) cost_to_come
--    case fst finalMP of
--        Nothing -> return (0)
--        Just finalCost -> do
--            let ctc = snd finalMP
--            --let wasVisited ((_,_,_),c) = c /= -1
--            --let seed = filter wasVisited [((ie,je,d), ctc!(ie,je,d)) | d<-[0..3]]
--            let seed = [((ie,je,d), finalCost)| d<-[0..3]]
--            let covered_maze_cells = processCostToCome ctc arr seed
--            return (length covered_maze_cells)
--    
--
-- 
--getProgenetor:: SearchState -> MazeState -> StateCost -> [StateCost]
--getProgenetor ctc ms curr_head = do
--    let ((x,y,d), c) = curr_head
--    let nextStateCosts = getUnActions ms (x,y,d) c
--    filter cmpCost nextStateCosts where
--        cmpCost ((xx,yy,dd),cc) = ctc!(xx,yy,dd) == cc
--
--processCostToCome :: SearchState -> MazeState -> [StateCost] -> Set MazeCell
--processCostToCome ctc ms curr_heads = do
--    let progenetorSet = concat $ map extend curr_heads where
--        extend path = getProgenetor ctc ms path 
--    let getInds ((i,j,_),_) = (i,j)
--    case progenetorSet of
--        [] -> S.fromList (map getInds curr_heads)
--        _ -> S.union (S.fromList (map getInds curr_heads)) (processCostToCome ctc ms progenetorSet)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
parseData :: [String] -> Int -> Int -> MazeState
parseData strs sz num = do
    let readInt y = read y ::Int
    let get_xy a = (head$tail b, head b) where b = map readInt (wordsWhen (==',') a)
    let hitsxy = map get_xy strs
    let arr = A.array ((0,0), (sz, sz)) [((rowIndex, colIndex), '.') | rowIndex <- [0..sz], colIndex <- [0..sz]]
    let arr_fallen = arr A.// zip (take num hitsxy) (repeat '#')
    arr_fallen

loadTestData :: IO MazeProblem
loadTestData = do
    fileStr <- readFile "data/test.txt"
    let fileLines = lines fileStr
    let arr = parseData fileLines 6 12
    return (arr, (0,0), (6,6))

loadProblem :: IO MazeProblem
loadProblem = do
    fileStr <- readFile "data/input.txt"
    let fileLines = lines fileStr
    let arr = parseData fileLines 70 1024 
    return (arr, (0,0), (70,70))

prepAndTestBlocked:: [String] -> Int -> Int -> IO (Maybe String)
prepAndTestBlocked strs sz num= do
    let arr = parseData strs sz num
    res <- brushfireMaze (arr, (0,0),(sz,sz))
    case res of
        Just x -> do
            print ("With the addition of wall at " ++ show (strs!!num) ++ " still solved the maze with steps = " ++ show x)
            prepAndTestBlocked strs sz (num+1)
        Nothing -> return (Just (strs!!(num-1)))

part2 :: String -> Int -> Int -> IO (Maybe String)
part2 filename sz start = do
    fileStr <- readFile filename
    let fileLines = lines fileStr
    prepAndTestBlocked fileLines sz start




testPart2 :: IO (Maybe String)
testPart2 = part2 "data/test.txt" 6 12

runPart2 :: IO (Maybe String)
runPart2 = part2 "data/input.txt" 70 1024

prettyPrint :: MazeState -> IO ()
prettyPrint arr = do
    let elems = A.elems arr
    let bds = A.bounds arr
    let ((_,x0),(_,xf)) = bds
    let width = xf - x0 + 1
    let strs = chunksOf width elems 
    putStr $ unlines strs
    return ()
