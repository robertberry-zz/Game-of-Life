-- Conway's Game of Life

import Data.Array.IArray
import System.Random
import Control.Applicative
import Control.Concurrent
import Control.Monad.State
import Control.Monad
import Data.List

newtype Grid = Grid { cells :: Array (Int, Int) Bool }
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
           
msBetweenIterations = 2000

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

neighbours :: Array (Int, Int) a -> Array (Int, Int) [a]
neighbours a = array (bounds a) (map (\c -> (c, neighboursFor c)) (indices a))
  where ((minX, minY), (maxX, maxY)) = bounds a
        wrap :: Int -> Int -> Int -> Int
        wrap min max n = mod (n - min) (max - min) + min
        wrapX = wrap minX maxX
        wrapY = wrap minY maxY
        neighbourIndices x y = [((wrapX n), (wrapY n)) | (n, m) <-
                                   [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]]
        neighboursFor (x, y) = map (a !) (neighbourIndices x y)
        
-- Lives on in the next generation given whether alive now and how many neighbours alive
lives :: Bool -> Int -> Bool
lives True 2 = True
lives _ 3 = True
lives True _ = False
lives _ _ = False

argAnd :: (a -> b) -> a -> (a, b)
argAnd f a = (a, f a)

-- Next iteration of the grid
nextGrid :: Grid -> Grid
nextGrid (Grid a) = Grid $ array (bounds a) (map (argAnd alive) (indices a))
                    where alive coord = lives (a ! coord) (neighboursAlive ! coord)
                          neighboursAlive = amap (length . filter id) (neighbours a)

printLoop :: Grid -> IO ()
printLoop grid = do 
  print grid
  printLoop (nextGrid grid)

main = do
  randomGen <- getStdGen
  let (initialGrid, _) = runState (randomGrid (10, 10)) randomGen in
    printLoop initialGrid
