module Lib
    ( someFunc
    ) where
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Foldable
-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec as Parsec

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))
import Text.Parsec.Char
import Text.Parsec.String

import qualified Data.Array as Array
import qualified Data.Set as Set
someFunc :: IO ()
someFunc = putStrLn "someFunc"

parseEofOrNewline :: Parser ()
parseEofOrNewline = Parsec.eof Parsec.<|> void (Parsec.char '\n')

pInt :: Parser Int
pInt = Parsec.try pPos Parsec.<|> pNeg
  where
    -- Parse positive number
    pPos = do 
      s <- Parsec.many1 Parsec.digit 
      return $ read s

    -- Parse negative number 
    pNeg = do
      Parsec.char '-'
      n <- pPos
      return (-n)

data Robot = Robot {robotPosition::(Int, Int), robotVelocity:: (Int, Int)} deriving (Show)
parseRobot ::Parser Robot
parseRobot = do
    Parsec.string "p="
    x <- pInt --read <$> Parsec.many1 digit
    Parsec.char ','
    y <- pInt --read <$> Parsec.many1 digit
    --bn <- A <$ Parsec.char 'A' Parsec.<|> B <$ Parsec.char 'B'
    Parsec.string " v="
    dx <- pInt -- read <$> Parsec.many1 digit
    Parsec.char ','
    dy <- pInt -- read <$> Parsec.many1 digit
    parseEofOrNewline
    return Robot {robotPosition=(x,y), robotVelocity=(dx,dy)}

parseRobots :: Parser [Robot]
parseRobots = do Parsec.many parseRobot

stepTimeRobot :: (Int, Int) -> Int -> Robot -> Robot
stepTimeRobot dims t robot = Robot {robotPosition=(xn,yn), robotVelocity=robotVelocity robot} where
    (x,y) = robotPosition robot
    (dx,dy) = robotVelocity robot
    xn = (x + t*dx) `mod` snd dims
    yn = (y + t*dy) `mod` fst dims

stepTimeRobots :: (Int, Int) -> Int -> [Robot] -> [Robot]
stepTimeRobots dims t = map (stepTimeRobot dims t) 

countRobotsInRange :: ((Int, Int),(Int,Int)) -> [Robot] -> Int
countRobotsInRange lims = length . filter f where
    f r = do 
        let (rx, ry) = robotPosition r
        let ((ymin, xmin), (ymax, xmax)) = lims
        rx <= xmax && rx >= xmin && ry <= ymax && ry >= ymin 
        
        

loadData :: String -> IO [Robot]
loadData filename = do
    fileStr <- readFile filename
    case Parsec.parse parseRobots "" fileStr of
        Left err -> error $ " Parse error " ++ show err
        Right robots -> return robots

getSafetyFactor :: (Int, Int) -> [Robot] -> Int
getSafetyFactor lims robots = do
    let (rows, cols) = lims
    let mid_row = floor (fromIntegral rows / 2) --3
    let mid_col = floor (fromIntegral cols / 2) -- 5
    
    let quad1 = countRobotsInRange ((0,0), (mid_row-1,mid_col-1)) robots
    let quad2 = countRobotsInRange ((0,mid_col+1), (mid_row-1,cols)) robots
    let quad3 = countRobotsInRange ((mid_row+1,0), (rows,mid_col-1)) robots
    let quad4 = countRobotsInRange ((mid_row+1,mid_col+1), (rows, cols)) robots
    quad1*quad2*quad3*quad4

getSafetyFactorPart2 :: (Int, Int) -> [Robot] -> Int
getSafetyFactorPart2 lims robots = do
    -- Computing a version of safety factor with 9 groups instead of 4
    let (rows, cols) = lims
    let r1 = floor (fromIntegral rows / 3)
    let c1 = floor (fromIntegral cols / 3)
    let r2 = floor (2 * fromIntegral rows / 3)
    let c2 = floor (2 * fromIntegral cols / 3)
    let quad1 = countRobotsInRange ((0,0), (r1-1,c1-1)) robots
    let quad2 = countRobotsInRange ((0,c1+1), (r1-1,c2-1)) robots
    let quad3 = countRobotsInRange ((0,c2+1), (r1-1,cols)) robots
    let quad4 = countRobotsInRange ((r1+1,0), (r2-1,c1-1)) robots
    let quad5 = countRobotsInRange ((r1+1,c1+1), (r2-1,c2-1)) robots
    let quad6 = countRobotsInRange ((r1+1,c2+1), (r2-1,cols)) robots
    let quad7 = countRobotsInRange ((r2+1,0), (rows, c1-1)) robots
    let quad8 = countRobotsInRange ((r2+1,c1+1), (rows, c2-1)) robots
    let quad9 = countRobotsInRange ((r2+1,c2+1), (rows, cols)) robots
    quad1*quad2*quad3*quad4*quad5*quad6*quad7*quad8*quad9


testPart1 :: IO Int
testPart1 = do
    robots <- loadData "data/test.txt"
    let rows = 7
    let cols = 11
    
    let robots100 = stepTimeRobots (rows, cols) 100 robots
    return (getSafetyFactor (rows, cols) robots100)

runPart1 :: IO Int
runPart1 = do
    robots <- loadData "data/input.txt"
    let rows = 103
    let cols = 101
    
    let robots100 = stepTimeRobots (rows, cols) 100 robots
    return (getSafetyFactor (rows, cols) robots100)

getSafetyFactorPart2AtTime :: [Robot] -> (Int, Int) -> Int -> Int
getSafetyFactorPart2AtTime robots lims t = getSafetyFactorPart2 lims robots_t
    where robots_t = stepTimeRobots lims t robots

getSafetyFactorPart2AtTimePair :: [Robot] -> (Int, Int) -> Int -> (Int, Int)
getSafetyFactorPart2AtTimePair robots lims t = (getSafetyFactorPart2AtTime robots lims t, t)
    
checkMin :: (Int, Int) -> (Int, Int) -> (Int, Int)
checkMin curr_min val = do
    let (min_ent, min_time) = curr_min
    let (ent, t) = val
    if ent < min_ent then do
--        print "Encountered new minimum entropy of " ++ show ent ++ " at step " ++ show t
--        return val
        val
    else curr_min --return curr_min

drawRobot :: Set.Set(Int, Int) -> Int -> Int -> IO()
drawRobot robotSet row col = do
    if Set.member (row,col) robotSet then putChar '#' else putChar '.'
    return ()

drawRobotLine :: Set.Set (Int, Int) -> Int -> Int -> [IO()]
drawRobotLine robotSet row cols = do
    let col = [j | j <- [0..cols]]
    map (drawRobot robotSet row) col


drawRobots :: [Robot] -> (Int, Int) -> [[IO ()]]
drawRobots robots lims = do
    let robSet = Set.fromList (map robotPosition robots)
    let (rows,cols) = lims
    let row = [i | i <- [0..rows]]
    map (\x -> drawRobotLine robSet x cols) row
    

runPart2 :: IO Int
runPart2 = do
    robots <- loadData "data/input.txt"
    let rows = 103
    let cols = 101
    let lims = (rows, cols)
    let ent_steps = map (getSafetyFactorPart2AtTimePair robots lims) [1..]
    let maxSteps = lcm rows cols
    let ent0 = getSafetyFactorPart2AtTime robots lims 0
    return (snd $ foldl' checkMin (ent0, 0) (take maxSteps ent_steps))