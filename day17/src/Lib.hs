module Lib
    (runTest1, runPart1, runPart2
    ) where
import Debug.Trace

import Data.Bits
import Data.Maybe
import Data.List
import Control.Monad (forM, when)
import Control.Monad.State (State, get, put, execState, StateT, execStateT, runStateT, lift, runState)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data ControlContext = ControlContext { rAX :: !Int,
                                       rBX :: !Int,
                                       rCX :: !Int,
                                       rIP :: !Int,
                                       output :: ![Int]} deriving (Show)
axIter :: Int -> Int
axIter a = a `div` 8

bxIter :: Int -> Int -> Int
bxIter a b = do--(((a mod 8) xor 3) xor 5) xor (a div 2^((a mod 8) xor 3))
    let amod8 = mod a 8
    let amod8xor3 = xor amod8 3
    let am8xor3xor5 = xor amod8xor3 5
    let powamod8xor3 = 2 ^ amod8xor3
    let adivpow = div a powamod8xor3
    xor am8xor3xor5 adivpow

test2Step :: (Int, Int) -> (Int, Int)
test2Step (ax1, bx1) = do
    let ax2 = axIter ax1
    let bx2 = bxIter ax1 bx1
    (ax2, bx2)

test2CondFail :: (Int, Int) -> Int -> Bool
test2CondFail (a, b) v =
    mod b 8 /= v || a == 0

test2CondFailLast :: (Int, Int) -> Int -> Bool
test2CondFailLast (a, b) v =
    mod b 8 /= v || a /= 0

runTest2Fast:: Int -> Bool
runTest2Fast a = do
    let cc = (a, 0)
    test2Fast cc [2,4,1,3,7,5,0,3,1,5,4,4,5,5,3,0]

unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse xs)

findMinAx :: [Int] -> [Int] -> Int -> Maybe Int
findMinAx vs vf seed = case unsnoc vs of
        Nothing -> Nothing
        Just (vt, v) -> do let opts = trace ("Have seed == " ++ show seed) [seed + y | y<-[0..7]]
                           let valid_opts = trace ("Next seeds " ++ show opts) filter (runTest2FastPartial (v:vf)) opts
                           if null valid_opts then Nothing else if null vt then Just (minimum valid_opts) else do
                               let opts_deeper = [shift y 3 | y<-valid_opts]
                               let opts_checked = trace ("Going deeper with opts = " ++ show opts_deeper) map (findMinAx vt (v:vf) ) opts_deeper
                               let valid_opts = catMaybes opts_checked
                               if null valid_opts then Nothing else Just (minimum valid_opts)

runTest2FastPartial:: [Int] -> Int -> Bool
runTest2FastPartial v a = do
    let cc = (a, 0)
    trace ("Testing " ++ show a ) test2Fast cc v

checkAxFastLims:: (Int, Int) -> [Int] -> Maybe Int
checkAxFastLims (a,b) v = do
    find (runTest2FastPartial v) [a..b]

searchAxFast :: [Int] -> Maybe Int
searchAxFast v = do
    find (runTest2FastPartial v) [0..]

test2Fast :: (Int, Int) -> [Int] -> Bool
test2Fast cc vs= do
    let cc2 = trace ("Stepping from " ++ show cc) test2Step cc
    case vs of
        [] -> trace("Ran out of vs") True
        --[v] -> not (test2CondFail cc2 v) -- if want to keep going
        [v] -> not (test2CondFailLast cc2 v)
        (v:vs') -> trace ("Many element case, checking " ++ show v) not (test2CondFail cc2 v) && test2Fast cc2 vs'


runTest1 :: IO ()
runTest1 = do
--Register A: 729
--Register B: 0
--Register C: 0
--
--Program: 0,1,5,4,3,0
    let cc = ControlContext 729 0 0 0 [] 
    let inp = [0,1,5,4,3,0]
    final_cc <- runProgram cc inp
    print (show final_cc)

runPart1 ::IO ()
runPart1 = do
--Register A: 55593699
--Register B: 0
--Register C: 0
--
--Program: 2,4,1,3,7,5,0,3,1,5,4,4,5,5,3,0
    let cc = ControlContext 55593699 0 0 0 [] 
    let inp = [2,4,1,3,7,5,0,3,1,5,4,4,5,5,3,0]
    final_cc <- runProgram cc inp
    print (show final_cc)

runPart1Ax :: Int -> IO ()
runPart1Ax ax = do
--Register A: ax
--Register B: 0
--Register C: 0
--
--Program: 2,4,1,3,7,5,0,3,1,5,4,4,5,5,3,0
    let cc = ControlContext ax 0 0 0 [] 
    let inp = [2,4,1,3,7,5,0,3,1,5,4,4,5,5,3,0]
    final_cc <- runProgram cc inp
    print (show final_cc)

tryPart2Test ::Int -> IO(Bool)
tryPart2Test ax = do
    let cc = trace ("Trying " ++ show ax) ControlContext ax 0 0 0 []
    let inp = [0,3,5,4,3,0]
    cc2 <- runProgram cc inp
    return (output cc2 == inp)

tryPart2 ::Int -> IO(Bool)
tryPart2 ax = do
    let cc = trace ("Trying " ++ show ax) ControlContext ax 0 0 0 []
    let inp = [2,4,1,3,7,5,0,3,1,5,4,4,5,5,3,0]
    cc2 <- runProgram cc inp
    return (output cc2 == inp)

-- Helper function to find the first `True` result from a list of IO Bool actions
findM :: (a -> IO Bool) -> [a] -> IO (Maybe a)
findM _ [] = return Nothing
findM p (x:xs) = do
    res <- p x
    if res then return (Just x) else findM p xs

runPart2Test ::IO (Maybe Int)
runPart2Test = do
    result <- findM tryPart2Test [0..]
    return result

runPart2 ::Maybe Int
runPart2 = do
--    result <- findM tryPart2 [0..]  -- LoL there is no way - this would run for like 10 years...-
   findMinAx [2,4,1,3,7,5,0,3,1,5,4,4,5,5,3,0] [] 0
    
runProgram :: ControlContext -> [Int] -> IO ControlContext
runProgram cc inp = do
    finalCC <- runStateT (executeProgram inp) cc
    return (snd finalCC)

executeProgram :: [Int] -> StateT ControlContext IO()
executeProgram inp = do
    cc <- get
    if rIP cc >= length inp then return()
    else do 
        let opcode = inp !! rIP cc
        let operand = inp!!(rIP cc + 1)
        
        let cc2 = handleInstruction cc opcode operand
        put cc2
        executeProgram inp
    

comboOperand :: Int -> ControlContext -> Int
comboOperand operand cc = case operand of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 3
    4 -> rAX cc
    5 -> rBX cc
    6 -> rCX cc
    7 -> error "Undefined combo operand value 7"
    _ -> error "Combo operand not 3 bits"

handleInstruction :: ControlContext -> Int -> Int -> ControlContext
handleInstruction cc opcode operand = do
    case opcode of
        0 -> adv operand cc -- adv, perform division of register A/2^combo operand
        1 -> bxl operand cc -- bxl, bitwise xor of register B and literal operand -> register B
        2 -> bst operand cc -- bst, put combo operand mod 8 into register B
        3 -> jnz operand cc -- jnz do nothing if register A is zero, otherwise update IP to the literal operand (dont increase ip)
        4 -> bxc operand cc -- bxc bitwise xor of register B and register C -> register B
        5 -> out operand cc -- out, outputs the value of the combo operand
        6 -> bdv operand cc -- bdv, same as adv but writes to register B (still loads from A)
        7 -> cdv operand cc -- cdv, same as adv but writes to register C (still loads from A)
        _ -> error ("Undefined opcode " ++ show opcode)

-- adv, perform division of register A/2^combo operand
adv :: Int -> ControlContext -> ControlContext
adv operand cc = do
    let coperand = comboOperand operand cc
    let coperand_modified = 2 ^ coperand
    let newrAX = rAX cc `div` coperand_modified
    let cc2 = cc {rAX = newrAX, rIP = rIP cc + 2}
    cc2

-- bxl, bitwise xor of register B and literal operand -> register B
bxl :: Int -> ControlContext -> ControlContext
bxl operand cc = do
    let bX = rBX cc
    let newBX = xor bX operand
    cc {rIP = rIP cc + 2, rBX = newBX}

-- bst, put combo operand mod 8 into register B
bst :: Int -> ControlContext -> ControlContext
bst operand cc = do
    let coperand = comboOperand operand cc
    let newBX = mod coperand 8
    cc {rIP = rIP cc + 2, rBX = newBX}

-- jnz do nothing if register A is zero, otherwise update IP to the literal operand (dont increase ip)
jnz :: Int -> ControlContext -> ControlContext
jnz operand cc = do
    if rAX cc /=0 then cc {rIP = operand}
    else cc {rIP = rIP cc + 2}

-- bxc bitwise xor of register B and register C -> register B
bxc :: Int -> ControlContext -> ControlContext
bxc _ cc = do
    let newrBX = xor (rBX cc) (rCX cc)
    cc {rIP = rIP cc + 2, rBX = newrBX}

-- out, outputs the value of the combo operand
out :: Int -> ControlContext -> ControlContext
out operand cc = do
    let coperand = comboOperand operand cc
    let coperand_mod = mod coperand 8
    cc {rIP = rIP cc + 2, output = output cc ++ [coperand_mod]}

-- bdv, same as adv but writes to register B (still loads from A)
bdv :: Int -> ControlContext -> ControlContext
bdv operand cc = do
    let coperand = comboOperand operand cc
    let coperand_modified = 2 ^ coperand
    let newrBX = rAX cc `div` coperand_modified
    let cc2 = cc {rBX = newrBX, rIP = rIP cc + 2}
    cc2

-- cdv, same as adv but writes to register C (still loads from A)
cdv :: Int -> ControlContext -> ControlContext
cdv operand cc = do
    let coperand = comboOperand operand cc
    let coperand_modified = 2 ^ coperand
    let newrCX = rAX cc `div` coperand_modified
    let cc2 = cc {rCX = newrCX, rIP = rIP cc + 2}
    cc2
