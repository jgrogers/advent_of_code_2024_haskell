{-# OPTIONS_GHC -Wno-x-partial #-}

module GridParse (listCharCoord, charCoordMap, gridSize) where

import qualified Data.Map as M

gridSize :: String -> (Int, Int)
gridSize input = (length (lines input), length (head (lines input)))

indexList :: [a] -> [(Int, a)]
indexList = zip [0 ..]

listCharCoord :: String -> [(Char, (Int, Int))]
listCharCoord input = do
    (r, row) <- indexList (lines input)
    (c, ch) <- indexList row
    return (ch, (r, c))

charCoordMap :: String -> M.Map Char [(Int, Int)]
charCoordMap = M.fromListWith (++) . map (\(ch, rc) -> (ch, [rc])) . listCharCoord
