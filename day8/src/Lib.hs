module Lib
    ( someFunc
    ) where
import Data.List.Split
import Data.List
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Distribution.Simple.CCompiler (filenameCDialect)
import qualified Data.ByteString.Builder.Prim as Data.Set
import System.Posix (seekDirStream)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

filterFunc :: (Char) -> (Integer, Integer, Char) -> Bool
filterFunc matchChar element = do
    let (i, j, ch) = element
    ch /= matchChar

addToMap :: Map Char [(Integer, Integer)] -> (Integer, Integer, Char)-> Map Char[(Integer, Integer)]
addToMap m el = do
    let (i, j, c) = el
    Map.insertWith (++) c [(i,j)] m

getAllPairs :: [(Integer, Integer)] -> [((Integer,Integer), (Integer,Integer))]
getAllPairs a = do
    let pairsHead = map (\b -> (head a, b)) (tail a)
    case tail a of
        [] -> pairsHead
        _ -> pairsHead ++ getAllPairs (tail a)

loadRawData :: String -> IO ([(Integer, Integer, Char)], (Int, Int))
loadRawData filename = do
    inputText <- readFile filename
    let fileLines = lines inputText
    let fileElemsIndex = zip [0..] (map (zip [0..]) fileLines)
    let elemsCoords = map (\x -> map (\y ->  (fst y, fst x, snd y)) (snd x)) fileElemsIndex
    return (concat elemsCoords, (length $ head fileLines, length fileLines))

getAllPairsFull :: Map Char [(Integer, Integer)] -> [((Integer,Integer), (Integer, Integer))]
getAllPairsFull m = concat $ map getAllPairs (Map.elems m)

getAntinodes :: ((Integer, Integer), (Integer, Integer)) -> [(Integer, Integer)]
getAntinodes p = do
    let (p1, p2) = p
    let (p11, p12) = p1
    let (p21, p22) = p2
    let d1 = p21 - p11
    let d2 = p22 - p12
    [(p21 + d1, p22 + d2), (p11 - d1, p12 - d2)]

part2AntinodeHelper :: (Integer, Integer) -> (Integer, Integer) -> (Int, Int) -> [(Integer, Integer)]
part2AntinodeHelper seed diff lims = do
    if filterOffGrid lims seed then do
        let (p1, p2) = seed
        let (d1, d2) = diff
        seed : part2AntinodeHelper (p1+d1, p2+d2) diff lims
    else []


getPart2Antinodes :: ((Integer, Integer), (Integer, Integer)) -> (Int, Int) -> [(Integer, Integer)]
getPart2Antinodes p lims = do
    let (p1, p2) = p
    let (p11, p12) = p1
    let (p21, p22) = p2
    let d1 = p21 - p11
    let d2 = p22 - p12
    let nd1 = p11 - p21
    let nd2 = p12 - p22
    let ascending = part2AntinodeHelper p2 (d1, d2) lims
    let descending = part2AntinodeHelper p1 (nd1, nd2) lims
    ascending ++ descending

filterOffGrid :: (Int, Int) -> (Integer, Integer) -> Bool
filterOffGrid lims a = do
    let (a1, a2) = a
    let (lim1, lim2) = lims
    not (a1 < 0 || a2 < 0 || a1 >= fromIntegral lim1 || a2 >= fromIntegral lim2)

loadData :: String -> IO (Map Char [(Integer,Integer)], (Int, Int))
loadData filename = do
    (elemsCoords, lims) <- loadRawData filename
    let elemCoordsFiltered = filter (filterFunc '.') elemsCoords
    let elemMap = foldl addToMap Map.empty elemCoordsFiltered
    return (elemMap, lims)
    
    
getPart1Ans :: String -> IO Int
getPart1Ans filename = do
    (m, lims) <- loadData filename
    let pairs = getAllPairsFull m
    let an = concatMap getAntinodes pairs
    let filtered = filter (filterOffGrid lims) an
    let s = Set.fromList filtered
    return (length (Set.elems s))

getPart2Ans :: String -> IO Int
getPart2Ans filename = do
    (m, lims) <- loadData filename
    let pairs = getAllPairsFull m
    let an = concatMap (\x -> getPart2Antinodes x lims) pairs
    let filtered = filter (filterOffGrid lims) an
    let s = Set.fromList filtered
    return (length (Set.elems s))
