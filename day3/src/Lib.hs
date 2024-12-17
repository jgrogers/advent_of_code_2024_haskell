module Lib
    ( matchRegex,
      getMulNumsRegex,
      stripDisabledMuls,
      removeDisabledMuls
    ) where
import Data.Typeable
import Text.Read (read)
import Debug.Trace
import Text.Regex.Posix

matchRegex :: String -> String -> [String]
matchRegex inputText regexStr = do
    let outputs = getAllTextMatches $ (inputText =~ regexStr) :: [String]
    outputs

getMulNumsRegex :: String -> [Int]
getMulNumsRegex inputMulCommand = do
    let outputs = getAllTextMatches $ (inputMulCommand =~ "[0-9]+")
    map (read :: String->Int) outputs

stripDisabledMuls :: [String] -> [String]
stripDisabledMuls inputMulCommands = do
    map suppress inputMulCommands where
        suppress s = do
            case s of 
                "don't()" -> ""
                "do()" -> ""
                otherwise -> s

removeDisabledMuls' :: [String]-> Bool -> [String]
removeDisabledMuls' inputMulCommands  enabled = do
    let command = head inputMulCommands
    if not enabled then case tail inputMulCommands of
        [] -> []
        otherwise -> case command of 
            "do()" -> trace "Enabling" removeDisabledMuls' (tail inputMulCommands) True
            _ -> removeDisabledMuls' (tail inputMulCommands) enabled 
    else case command of
        "don't()" -> trace "Disabling" removeDisabledMuls' (tail inputMulCommands) False 
        "do()" -> trace "Enabling" removeDisabledMuls' (tail inputMulCommands) True 
        otherwise -> case tail inputMulCommands of
            [] -> [command]
            otherwise -> command : removeDisabledMuls' (tail inputMulCommands) enabled

removeDisabledMuls :: [String] -> [String]
removeDisabledMuls inputMulCommands = removeDisabledMuls' inputMulCommands True