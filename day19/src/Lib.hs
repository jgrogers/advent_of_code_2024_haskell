module Lib
    ( testPart2,
    runPart1,
    testPart1
    ) where
import Data.List
import Data.Array ((!), Array)
import qualified Data.Array as A
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.Regex.Posix
import Debug.Trace
import Control.Parallel.Strategies
import Data.Sequence (Seq, (!?))
import qualified Data.Sequence as Seq
import Data.Char (ord)
import GHC.Conc (numCapabilities)

matchRegex :: String -> String -> Bool
matchRegex inputText regexStr = do
--    let outputs = getAllTextMatches $ (inputText =~ regexStr) :: [String]
 --   concat outputs
    inputText =~ regexStr

stripCommas :: String -> String
stripCommas = filter (/= ',')

parseString :: String -> [String]
parseString = map stripCommas . words

loadData :: String -> IO ([String],[String])
loadData filename = do
    fileStrs <- readFile filename
    let fileLines = lines fileStrs
    return (parseString $ head fileLines, tail$tail fileLines)
    
getArrayOfTowels :: [String] -> Array Int (Seq Char)
getArrayOfTowels towels = A.listArray (0,length towels-1) (map Seq.fromList towels)

getMapOfTowelInds :: Array Int (Seq Char) -> Array Char [Int]
getMapOfTowelInds arr = 
    let aa = A.listArray ('a','z') (repeat [])
        firstChar wd = wd `Seq.index` 0
        mergeify (ind, wd) ar = ar A.// [(firstChar wd, ind:ar!firstChar wd)]
    in foldr mergeify aa (A.assocs arr)

formRegexStr :: [String] -> String
formRegexStr towels = 
    let prep_tail_towels = map (")|("++) (tail towels)
        prefix_towels = foldl' (++) ("^(("++head towels) prep_tail_towels
    in prefix_towels ++ "))+$"

checkMatches :: [String] -> String -> Int
checkMatches rainbows reg = 
    let m = map (`matchRegex` reg) rainbows
        f = filter (`matchRegex` reg) rainbows
    in length f
--    let z = zip m rainbows
--    let matches = filter cmpPair z where cmpPair (a,b) = a == b
--    length matches

getMatches :: [String] -> String -> [String]
getMatches rainbows reg =
    let m = map (`matchRegex` reg) rainbows
    in filter (`matchRegex` reg) rainbows

doPart1 :: String -> IO Int
doPart1 filename = do
    (towels, rainbows) <- loadData filename
    let reg = formRegexStr towels
    return (checkMatches rainbows reg)

testPart1 :: IO Int
testPart1 = doPart1 "data/test.txt"

runPart1 :: IO Int
runPart1 = doPart1 "data/input.txt"


--Rework this , instead of tracking the list of active state sequences (and index in terminal state)
-- track a set of current states, indexs in that state, and number of copies of that state currently going
-- Then this set can't get bigger than #towels * len (biggest towel) which is much smaller than the thing I was doing. probably size of universe...

checkTerminal :: Array Int (Seq Char) -> (Int, Int) -> Int -> Bool
checkTerminal towels state _ =
    let (st, ind) = state
    in ind == Seq.length (towels!st)

stepCharForState :: Array Int (Seq Char) -> Array Char [Int] -> Char -> (Int, Int) ->  Int -> Map (Int, Int) Int
stepCharForState towels dic chr state multiplicity = 
    let (curr_matcher, ind) = state
    in if curr_matcher == -1 || Seq.length(towels!curr_matcher) == ind then -- Add on matching states starting from this one
        let expanding_states = dic!chr --filter towelStartsWith (A.assocs towels)
            --towelStartsWith (_, t) = t `Seq.index` 0 == chr
        in M.fromList [((es,1), multiplicity) | es <- expanding_states]
       else if towels!curr_matcher `Seq.index` ind ==chr then
            M.singleton (curr_matcher, ind + 1) multiplicity
       else M.empty
-- The list of states that I am in (sequence of match indices) and what character we are trying to match next
stepChar :: Map(Int, Int) Int -> Array Int (Seq Char) -> Array Char [Int] -> Char -> Map (Int, Int) Int
stepChar states towels dic chr =
    let next_states = M.mapWithKey (stepCharForState towels dic chr) states -- This is a map of maps...
    in foldr (M.unionWith (+)) M.empty next_states

traceNFA :: Map(Int,Int) Int-> Array Int (Seq Char) -> Array Char [Int] -> String -> Map (Int, Int) Int
traceNFA states towels dic str =
    case str of
        [] -> let terminal = M.filterWithKey (checkTerminal towels) states
                in trace ("Finished processing one") terminal
        s -> trace ("Tracing NFA on " ++ str ++ " with current states " ++ show states) traceNFA (stepChar states towels dic (head s)) towels dic (tail s)

    -- Go through each existing match state and see if it should be extended. Could also be ready to extend with another
countAcceptingSequences :: Array Int (Seq Char) -> Array Char [Int] -> String -> Int
countAcceptingSequences towels dic rainbow = 
    let accepting_sets = traceNFA (M.singleton (-1,0) 1) towels dic rainbow
        num_accepting_sets = M.foldr (+)  0 accepting_sets
    in trace ("Got " ++ show num_accepting_sets ++ " with " ++ show accepting_sets) num_accepting_sets

testPart2 :: String -> IO Int
testPart2 fname = do
    (towel_strs, rainbows) <- loadData fname
    let towels = getArrayOfTowels towel_strs
    let dic = getMapOfTowelInds towels
    --let as = map (countAcceptingSequences t) rainbows
    let f = map (countAcceptingSequences towels dic) rainbows
        cs = f `using` parList rdeepseq
    let s = sum cs
    return s