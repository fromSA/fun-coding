{-# LANGUAGE DeriveGeneric #-}
import Data.List (intercalate)
import System.Random (randomRIO)
import Control.Concurrent (threadDelay)

-- cellular automata in haskell

-- Data decleration
type Width = Int
type Height = Int
type Pos = (Int, Int)
type Perimeter = Pos
data Grid = Grid Perimeter [[Cell]]

data Cell = Cell State Pos
data State = Alive | Dead deriving (Eq)

--- Update

getPosition :: [[Cell]] -> Pos -> Cell
getPosition ls (a,b) = ls !! a !! b -- TODO This assumes that the position is in the range of the list. We could use Maybe monad, but for later.

applyUpdateRuleOnCell :: Cell -> [Cell] -> Cell
applyUpdateRuleOnCell (Cell s p) ps = Cell Alive p

applyUpdateRuleOnGrid :: [[Cell]] -> Cell -> [Pos] -> Cell
applyUpdateRuleOnGrid cells c ps = let neighbors = map (getPosition cells) ps
                             in applyUpdateRuleOnCell c neighbors

inn :: Int -> Perimeter -> Bool
inn x (a,b) = a <= x && x <= b

filterBoundaries :: Perimeter -> [Pos] -> [Pos]
filterBoundaries (w,h) = filter (\(x,y) ->  x `inn` (0, w) &&  y `inn` (0, h))

get8Neighbors :: Cell -> [Pos]
-- version1 get8Neighbors = [(x-1,y),(x+1,y), (x-1,y-1),(x,y-1),(x+1,y-1), (x-1,y+1),(x,y+1),(x+1,y+1)]
get8Neighbors (Cell s (x,y)) = [(x+dx, y+dy) |  dx <- [-1..1] , dy <- [-1..1], (dx, dy) /= (0, 0)]

updateCellState :: Perimeter -> [[Cell]] -> [[Cell]]
updateCellState perimeters cells =  map (map (\c -> (applyUpdateRuleOnGrid cells c . filterBoundaries perimeters . get8Neighbors) c)) cells

updateGrid:: Grid -> Grid
updateGrid grid = let Grid perimeters cells = grid 
                    in Grid perimeters (updateCellState perimeters cells)

-- Display
instance Show State where
    show s
        | s == Alive = "X"
        | s == Dead = " "

instance Show Cell where
    show (Cell s p) =  show s

showRow :: [Cell] -> String
showRow row = unwords (map show row)

printGrid :: Grid -> String
printGrid (Grid p cells) = intercalate "\n" (map showRow cells)

instance Show Grid where
    show = printGrid


--- Init grid
toState :: Int -> State
toState 1 = Alive
toState _ = Dead

randomState :: IO State
randomState = toState <$> randomRIO (0, 1) 


enumerateGrid :: Perimeter -> IO [[(Pos, State)]]
enumerateGrid (width, height) = mapM generateRow [0..height-1]
  where
    generateRow y = mapM (\x -> do
        state <- randomState
        return ((x, y), state)) [0..width-1]


toCell :: (Pos, State) -> Cell
toCell (p,s) = Cell s p

defGrid :: Int -> Int -> IO Grid
defGrid w h = do
            let perimeter = (w,h)
            grid <- enumerateGrid perimeter
            return $ Grid perimeter $ map (map toCell) grid


---

sleepSeconds :: Int -> IO ()
sleepSeconds seconds = threadDelay (seconds * 1000000)

clearScreen :: IO ()
clearScreen = do
    _ <- system "clear"  -- For Unix-like systems
    return ()

main :: Maybe Grid -> IO ()
main Nothing = do
    g <- defGrid 10 10
    main $ Just g
main (Just g) = do 
    print g
    sleepSeconds 2
    clearScreen
    main $ Just $ updateGrid g
    return ()
