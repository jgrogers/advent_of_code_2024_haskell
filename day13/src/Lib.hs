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
someFunc :: IO ()
someFunc = putStrLn "someFunc"
data ButtonName = A | B deriving (Show)
data ClawButton = ClawButton {buttonName:: ButtonName, clawX :: Int, clawY :: Int } deriving (Show)
parseButton ::Parser ClawButton
parseButton = do
    Parsec.string "Button "
    bn <- A <$ Parsec.char 'A' Parsec.<|> B <$ Parsec.char 'B'
    Parsec.string ": X+"
    x <- read <$> Parsec.many1 digit
    Parsec.string ", Y+"
    y <- read <$> Parsec.many1 digit
    Parsec.char '\n'
    return ClawButton {buttonName=bn, clawX=x, clawY=y}

--    where cb = ClawButton <$> X <$> Y
--        X = 5
--        Y = 3
data Prize = Prize {prizeX :: Int, prizeY :: Int } deriving (Show)
parsePrize ::Parser Prize
parsePrize = do
    Parsec.string "Prize: X="
    x <- read <$> Parsec.many1 digit
    Parsec.string ", Y="
    y <- read <$> Parsec.many1 digit
    Parsec.char '\n'
    return Prize {prizeX=10000000000000 + x, prizeY=10000000000000 + y}

data ClawMachine = ClawMachine {buttonA:: ClawButton, buttonB:: ClawButton, prize::Prize} deriving (Show)
parseClawMachine ::Parser ClawMachine
parseClawMachine = do
    button1 <- parseButton
    button2 <- parseButton
    prize <- parsePrize
    --Parsec.eof Parsec.<|> Parsec.char '\n'
    Parsec.char '\n'
    return ClawMachine {buttonA=button1, buttonB=button2, prize=prize}

parseClawMachines :: Parser [ClawMachine]
parseClawMachines = do Parsec.many parseClawMachine

loadData :: String -> IO [ClawMachine]
loadData filename = do
    fileString <- readFile filename
    case Parsec.parse parseClawMachines "" fileString of
        Left err -> error $ " Parse error " ++ show err
        Right clms -> return clms

checkAssignment :: ClawMachine -> (Int, Int) -> Bool
checkAssignment cm bps = do
    let (a,b) = bps
    let x = (clawX $ buttonA $ cm) * a + (clawX $ buttonB $ cm) * b
    let y = (clawY $ buttonA $ cm) * a + (clawY $ buttonB $ cm) * b
    (x,y) == (prizeX $ prize cm, prizeY $ prize cm)
   
findButtonAssignmentThatWorks :: ClawMachine -> Maybe (Int, Int)
findButtonAssignmentThatWorks cm = do
    let opts = [(i,j) | i <- [0..100], j <- [0..100]]
    find (checkAssignment cm) opts

fastButtonAssignment :: ClawMachine -> Maybe (Int, Int)
fastButtonAssignment cm = do
    let a = fromIntegral $ clawX $ buttonA cm
    let b = fromIntegral $ clawX $ buttonB cm
    let c = fromIntegral $ clawY $ buttonA cm
    let d = fromIntegral $ clawY $ buttonB cm
    let det = 1.0 / (a*d - b*c)
    let px = fromIntegral $ prizeX $ prize cm 
    let py = fromIntegral $ prizeY $ prize cm
    let b1 = d*det * px - b*det * py
    let b2 = a*det * py - c*det * px
    let b1_int = round b1
    let b2_int = round b2
    if checkAssignment cm (b1_int, b2_int) then Just (b1_int, b2_int) else Nothing
    
coinValue :: Maybe (Int, Int) -> Int
coinValue m = case m of
    Nothing -> 0
    Just (a,b) -> 3 * a + b