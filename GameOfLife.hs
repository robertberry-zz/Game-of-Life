-- Conway's Game of Life

import Data.Array.IArray
import System.Random
import Control.Applicative
import Control.Monad.State
import Control.Monad
import Data.List

newtype Grid = Grid { cells :: (Array (Int, Int) Bool) }
  deriving (Eq, Ord, Read)

data Coordinate = Coordinate { x :: Int, y :: Int }
  deriving (Eq, Ord, Read, Show)
           
-- There must be a way to make this nicer ... Ben James?
instance Show Grid where
  show g = unlines allLines
    where allLines :: [String]
          allLines = (topMargin : gridLines) ++ [topMargin]
          (_, (columns, rows)) = bounds (cells g)
          topMargin = ('+' : (take (columns + 1) $ repeat '-')) ++ "+"
          gridLines = [gridLine n | n <- [0..rows]]
          gridLine row = ('|' : [(if ((cells g) ! (column, row)) then '#' else ' ') | column <- [0..columns]]) ++ "|"
           
-- Helpful wrappers for Random typeclass in State monad
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt = state . randomR

nRandomRsSt :: (RandomGen g, Random a) => Int -> (a, a) -> State g [a]
nRandomRsSt 0 _ = return []
nRandomRsSt n (min, max) = do
  h <- randomRSt (min, max)
  t <- nRandomRsSt (n - 1) (min, max)
  return $ h : t

instance Random Coordinate where
  randomR (Coordinate minX minY, Coordinate maxX maxY) = 
    runState $ do
      x <- randomRSt (minX, maxX)
      y <- randomRSt (minY, maxY)
      return $ Coordinate x y
  
  random = randomR (Coordinate minBound minBound, Coordinate maxBound maxBound)
  
coordinates :: (Int, Int) -> [Coordinate]
coordinates (columns, rows) = Coordinate <$> [0..columns] <*> [0..rows]
  
-- Grid of given dimensions with a random number of cells initialized
randomGrid :: (RandomGen g) => (Int, Int) -> State g Grid
randomGrid (columns, rows) = 
  do
    initiallyAlive <- randomRSt (0, columns * rows)
    livingCells <- nRandomRsSt initiallyAlive (Coordinate 0 0, Coordinate columns rows)
    let initializations = [((x c, y c), c `elem` livingCells) | c <- coordinates (columns, rows)] in
      return $ Grid $ array ((0, 0), (columns, rows)) initializations

main = do
  randomGen <- getStdGen
  let (grid, _) = runState (randomGrid (10, 10)) randomGen
      in putStrLn $ show grid