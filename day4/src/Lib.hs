module Lib
    ( someFunc,
    loadData,
    countAllX_masses,
    countAllXmasses
    ) where
import Debug.Trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

loadData :: String -> IO [[Char]]
loadData filename = do
    inputText <- readFile filename
    return (lines inputText)

checkChar :: [[Char]] -> Int -> Int -> Char -> Bool
checkChar datablock i j chr = do
    if i < 0 then False
    else if j < 0 then False
    else if j >= length datablock then False
    else if i >= length (datablock !! 0) then False
    else datablock !! j !! i == chr

xmasDir :: [[Char]] -> Int -> Int -> Int -> Int -> Int
xmasDir datablock i j di dj = do
    case (di,dj) of
        (0,0) -> 0
        (_,_) -> do 
            let hasX = checkChar datablock i j 'X'
            let hasM = checkChar datablock (i+di) (j+dj) 'M'
            let hasA = checkChar datablock (i+di*2) (j+dj*2) 'A'
            let hasS = checkChar datablock (i+di*3) (j+dj*3) 'S'
            if hasX && hasM && hasA && hasS then 1 else 0

x_masij :: [[Char]] -> Int -> Int -> Int 
x_masij datablock i j = do
    let hasA = checkChar datablock i j 'A'
    let hasM1 = checkChar datablock (i-1) (j-1) 'M'
    let hasM2 = checkChar datablock (i+1) (j-1) 'M'
    let hasM3 = checkChar datablock (i-1) (j+1) 'M'
    let hasM4 = checkChar datablock (i+1) (j+1) 'M'
    let hasS1 = checkChar datablock (i-1) (j-1) 'S'
    let hasS2 = checkChar datablock (i+1) (j-1) 'S'
    let hasS3 = checkChar datablock (i-1) (j+1) 'S'
    let hasS4 = checkChar datablock (i+1) (j+1) 'S'
    let mas1fwd = hasA && hasM1 && hasS4
    let mas1rev = hasA && hasM4 && hasS1
    let mas2fwd = hasA && hasM3 && hasS2
    let mas2rev = hasA && hasM2 && hasS3
    if (mas1fwd || mas1rev ) && (mas2fwd || mas2rev) then 1 else 0
             

countXmasses :: [[Char]] -> Int -> Int -> Int
countXmasses datablock i j = do
    let dirs = [(-1), 0, 1] :: [Int]
    let counts = map (\dj -> map (\di -> xmasDir datablock i j di dj) dirs) dirs
    sum (map sum counts)
    
countAllXmasses :: [[Char]] -> Int
countAllXmasses datablock = do
    let rows = length datablock
    let cols = length (datablock !! 0)
    let xmasses = map (\j -> map (\i -> countXmasses datablock i j) [0..rows]) [0..cols]
    sum (map sum xmasses)

countAllX_masses:: [[Char]] -> Int
countAllX_masses datablock = do
    let rows = length datablock
    let cols = length (datablock !! 0)
    let xmasses = map (\j -> map (\i -> x_masij datablock i j) [0..rows]) [0..cols]
    sum (map sum xmasses)
