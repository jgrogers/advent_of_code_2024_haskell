module Main where

import qualified Data.Foldable as F
import qualified Data.Graph as G
import Data.List (groupBy, sort, sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Tree (flatten)

import GridParse (charCoordMap)

data Edge = TOP | BOTTOM | LEFT | RIGHT
    deriving (Show, Eq)

main :: IO ()
main = do
    f <- readFile "data/input.txt"
    cpm <- parseInput <$> getContents
    --cpm <- parseInput f
    putStrLn $ "Part 1: " ++ show (resolve cpm score_1)
    putStrLn $ "Part 2: " ++ show (resolve cpm score_2)

parseInput s = charCoordMap s

resolve cpm scoreFunc = sum . map (scoreChar scoreFunc cpm) . M.keys $ cpm

score_1 component = area component * perimiter component

score_2 component = area component * sidesCount component

perimiter component = foldl (\acc p -> acc + length (filter (not . (`elem` component)) $ adjacentCoords p)) 0 component

area :: (Foldable t) => t a -> Int
area = length

sidesCount c = sum $ map (\d -> sidesInDirection d c) [TOP, BOTTOM, LEFT, RIGHT]

adjacentCoords (r, c) = [(r + dr, c + dc) | (dr, dc) <- [(0, 1), (1, 0), (0, -1), (-1, 0)]]

neighbors points = filter (`elem` points) . adjacentCoords

notNeighbors points = filter (`notElem` points) . adjacentCoords

edge points p = (p, p, neighbors points p)

graph points = G.graphFromEdges (map (edge points) (F.toList points))

componentsFromGraph v = map (map (\(_, p, _) -> p) . map v . flatten) . G.scc

scoreChar scoreFunc cpm c = sum $ map scoreFunc cs
  where
    charPs = cpm M.! c
    pSet = S.fromList charPs
    (g, v, _) = graph pSet
    cs = componentsFromGraph v g

applyEdges ps = map (\p -> (p, concat (dirsForP ps p))) $ F.toList ps
  where
    dirsForP ps p@(r, c) = do
        (r', c') <- notNeighbors ps p
        case (r' - r, c' - c) of
            (0, 1) -> return [RIGHT]
            (0, -1) -> return [LEFT]
            (1, 0) -> return [BOTTOM]
            (-1, 0) -> return [TOP]

splitSublists = concatMap splitConsecutive

splitConsecutive :: [Int] -> [[Int]]
splitConsecutive [] = []
splitConsecutive (x : xs) = go [x] xs
  where
    go acc [] = [acc]
    go acc@(a : _) (y : ys)
        | y == a + 1 = go (y : acc) ys
        | otherwise = reverse acc : go [y] ys

sidesInDirection :: (Foldable t) => Edge -> t (Int, Int) -> Int
sidesInDirection dir =
    let
        (sortCoordsF, groupCoordF, coordSelectF) =
            if dir `elem` [LEFT, RIGHT]
                then
                    (comparing snd, (\(r, c) (r', c') -> c == c'), (\(r, c) -> r))
                else
                    (comparing fst, (\(r, c) (r', c') -> r == r'), (\(r, c) -> c))
     in
        length
            . splitSublists
            . map (sort . map coordSelectF)
            . groupBy groupCoordF
            . sortBy sortCoordsF
            . map (\(p, e) -> p)
            . filter (\(p, e) -> dir `elem` e)
            . applyEdges

{-
real    0m3.052s
user    0m3.006s
sys     0m0.048s
-}