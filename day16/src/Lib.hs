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
type Direction = Int
type MazeState = Array MazeCell Char
type MazeProblem = (MazeState, MazeCell, MazeCell)
type MazeCellDir = (Int, Int, Direction) -- Directions 0=east, 1=north, 2=west, 3=south
type StateCost = (MazeCellDir, Integer)
type SearchState = Array MazeCellDir Integer -- Cost to come to each search state
type MazePQ = MinQ.MinPQueue Integer MazeCellDir

getForwardAction :: StateCost -> StateCost
getForwardAction ((x,y,r), ctc) | r == 0 = ((x, y+1, r), ctc + 1)
                                | r == 1 = ((x-1, y, r), ctc + 1)
                                | r == 2 = ((x, y-1, r), ctc + 1)
                                | r == 3 = ((x+1, y, r), ctc + 1)
                                | otherwise = error("Unknown rotation" ++ show r)
getReverseAction :: StateCost -> StateCost
getReverseAction ((x,y,r), ctc) | r == 0 = ((x, y-1, r), ctc - 1)
                                | r == 1 = ((x+1, y, r), ctc - 1)
                                | r == 2 = ((x, y+1, r), ctc - 1)
                                | r == 3 = ((x-1, y, r), ctc - 1)
                                | otherwise = error("Unknown rotation" ++ show r)
getRotationActions :: StateCost -> [StateCost]
getRotationActions ((x,y,r), ctc) | r == 0 = [((x,y,1), ctc+1000), ((x,y,3), ctc+1000)]
                                  | r == 1 = [((x,y,2), ctc+1000), ((x,y,0), ctc+1000)]
                                  | r == 2 = [((x,y,3), ctc+1000), ((x,y,1), ctc+1000)]
                                  | r == 3 = [((x,y,0), ctc+1000), ((x,y,2), ctc+1000)]
                                  | otherwise = error("Unknown rotation" ++ show r)
getUnRotationActions :: StateCost -> [StateCost]
getUnRotationActions ((x,y,r), ctc) | r == 0 = [((x,y,3), ctc-1000), ((x,y,1), ctc-1000)]
                                    | r == 1 = [((x,y,0), ctc-1000), ((x,y,2), ctc-1000)]
                                    | r == 2 = [((x,y,1), ctc-1000), ((x,y,3), ctc-1000)]
                                    | r == 3 = [((x,y,2), ctc-1000), ((x,y,0), ctc-1000)]
                                    | otherwise = error("Unknown rotation" ++ show r)

getActions :: MazeState -> MazeCellDir -> Integer -> [StateCost] -- get the legal actions and incremental costs
getActions ms curr_state ctc = do
    let (x,y,r) = curr_state
    let rotation_actions_with_costs = getRotationActions (curr_state, ctc)
    let ((next_x, next_y, next_r), fwd_ctc) = getForwardAction (curr_state, ctc)
    if ms ! (next_x, next_y) /= '#' then ((next_x, next_y, r),fwd_ctc):rotation_actions_with_costs
    else rotation_actions_with_costs

getUnActions :: MazeState -> MazeCellDir -> Integer -> [StateCost] -- get the legal actions and incremental costs
getUnActions ms curr_state ctc = do
    let (x,y,r) = curr_state
    let unrotation_actions_with_costs = getUnRotationActions (curr_state, ctc)
    let ((next_x, next_y, next_r), fwd_ctc) = getReverseAction (curr_state, ctc)
    if ms ! (next_x, next_y) /= '#' then ((next_x, next_y, next_r),fwd_ctc):unrotation_actions_with_costs
    else unrotation_actions_with_costs

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
        let (x,y,r) = next_state
        let (gx, gy) = goal
        case best_so_far of
            Nothing -> do
                if x == gx && y == gy then brushfireMaze' mp remaining_queue (Just ctc)
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
    let cost_to_come = A.array ((i0, j0, 0), (i1, j1, 3)) [((i,j, k), -1)| i <-[i0..i1], j<-[j0..j1], k<-[0..3]]
    let (i,j) = s
    finalMP <- runStateT (brushfireMaze' mp (MinQ.singleton 0 (i,j,0)) Nothing) cost_to_come
    return (fst finalMP)

countOptimalCellsMaze :: MazeProblem -> IO (Int)
countOptimalCellsMaze mp = do
    let (arr, s, e) = mp
    let ((i0, j0),(i1,j1)) = A.bounds arr
    let cost_to_come = A.array ((i0, j0, 0), (i1, j1, 3)) [((i,j, k), -1)| i <-[i0..i1], j<-[j0..j1], k<-[0..3]]
    let (i,j) = s
    let (ie, je) = e
    finalMP <- runStateT (brushfireMaze' mp (MinQ.singleton 0 (i,j,0)) Nothing) cost_to_come
    case fst finalMP of
        Nothing -> return (0)
        Just finalCost -> do
            let ctc = snd finalMP
            --let wasVisited ((_,_,_),c) = c /= -1
            --let seed = filter wasVisited [((ie,je,d), ctc!(ie,je,d)) | d<-[0..3]]
            let seed = [((ie,je,d), finalCost)| d<-[0..3]]
            let covered_maze_cells = processCostToCome ctc arr seed
            return (length covered_maze_cells)
    

 
getProgenetor:: SearchState -> MazeState -> StateCost -> [StateCost]
getProgenetor ctc ms curr_head = do
    let ((x,y,d), c) = curr_head
    let nextStateCosts = getUnActions ms (x,y,d) c
    filter cmpCost nextStateCosts where
        cmpCost ((xx,yy,dd),cc) = ctc!(xx,yy,dd) == cc

processCostToCome :: SearchState -> MazeState -> [StateCost] -> Set MazeCell
processCostToCome ctc ms curr_heads = do
    let progenetorSet = concat $ map extend curr_heads where
        extend path = getProgenetor ctc ms path 
    let getInds ((i,j,_),_) = (i,j)
    case progenetorSet of
        [] -> S.fromList (map getInds curr_heads)
        _ -> S.union (S.fromList (map getInds curr_heads)) (processCostToCome ctc ms progenetorSet)
            
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

