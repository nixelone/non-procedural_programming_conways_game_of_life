{-# LANGUAGE RecordWildCards #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
-- import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), SpecialKey(..), KeyState(..))
import Graphics.Gloss.Interface.IO.Game()

-- Represent the world
data World = World
  { grid              :: [[Bool]]
  , running           :: Bool
  , tickInterval      :: Float
  , timeSinceLastTick :: Float
  , cellSize          :: Float
  }

-- Grid dimensions
numRows, numCols :: Int
numRows = 30
numCols = 30

-- Cell padding
padding :: Float
padding = 40  -- space around grid for text and nicer look

-- Window dimensions (based on cell size + padding)
windowWidth, windowHeight :: Int
windowWidth  = round (fromIntegral numCols * 20 + 1 * padding)
windowHeight = round (fromIntegral numRows * 20 + 2 * padding)

-- Empty grid
emptyGrid :: Int -> Int -> [[Bool]]
emptyGrid rows cols = replicate rows (replicate cols False)

-- Example grid: blinker in the middle
initialGrid :: [[Bool]]
initialGrid =
  let rows = numRows
      cols = numCols
      base = emptyGrid rows cols
      midR = rows `div` 2
      midC = cols `div` 2
  in [ [ if (r, c) `elem` [(midR-1, midC), (midR, midC), (midR+1, midC+1)]
          then True else base !! r !! c
       | c <- [0..cols-1] ]
     | r <- [0..rows-1] ]

initialWorld :: World
initialWorld = World
  { grid              = initialGrid
  , running           = False
  , tickInterval      = 0.3
  , timeSinceLastTick = 0
  , cellSize          = 20
  }

-- Render world as picture
render :: World -> Picture
render World{..} = Pictures $
  drawGrid grid cellSize ++
  [ translate (-fromIntegral windowWidth/2 + 10) (fromIntegral windowHeight/2 - 30) $
      scale 0.15 0.15 $
      color black $
      text (if running then "Running" else "Paused")
  ]

-- Draws the grid of cells
drawGrid :: [[Bool]] -> Float -> [Picture]
drawGrid g size =
  [ translate x y (color (cellColor alive) (rectangleSolid size size))
  | (row, r) <- zip g [0..]
  , (alive, c) <- zip row [0..]
  , let x = fromIntegral c * size - halfWidth
  , let y = fromIntegral (negate r) * size + halfHeight
  ]
  where
    halfWidth  = fromIntegral (numCols - 1) * size / 2
    halfHeight = fromIntegral (numRows - 1) * size / 2

cellColor :: Bool -> Color
cellColor True  = white               -- alive
cellColor False = black               -- dead

-- Handle keyboard/mouse input
handleInput :: Event -> World -> World
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) w@World{..} =
  let col = floor ((x + fromIntegral windowWidth  / 2) / cellSize) - 1
      row = numRows + 1 - floor ((y + fromIntegral windowHeight / 2) / cellSize)  -- flip Y
  in if row >= 0 && row < numRows && col >= 0 && col < numCols
       then w { grid = toggleCell row col grid }
       else w
handleInput (EventKey (SpecialKey KeySpace) Down _ _) w =
  w { running = not (running w) }
handleInput _ w = w

toggleCell :: Int -> Int -> [[Bool]] -> [[Bool]]
toggleCell row col grid =
  [ [ if r == row && c == col then not cell else cell
    | (c, cell) <- zip [0..] rowVals ]
  | (r, rowVals) <- zip [0..] grid ]

-- Update world every frame
update :: Float -> World -> World
update dt w
  | running w && timeSinceLastTick w + dt >= tickInterval w =
      w { grid              = nextGeneration (grid w)
        , timeSinceLastTick = 0
        }
  | running w =
      w { timeSinceLastTick = timeSinceLastTick w + dt }
  | otherwise = w

-- Compute next generation
nextGeneration :: [[Bool]] -> [[Bool]]
nextGeneration g =
  [ [ applyRules (g !! r !! c) (liveNeighbors r c)
    | c <- [0..cols-1] ]
  | r <- [0..rows-1] ]
  where
    rows = length g
    cols = length (head g)

    liveNeighbors r c =
      length [ () | dr <- [-1..1], dc <- [-1..1]
                  , not (dr == 0 && dc == 0)
                  , let r' = r + dr
                  , let c' = c + dc
                  , r' >= 0, r' < rows
                  , c' >= 0, c' < cols
                  , g !! r' !! c' ]

    applyRules alive n
      | alive && (n == 2 || n == 3) = True
      | not alive && n == 3         = True
      | otherwise                   = False

-- Main
main :: IO ()
main = play
         (InWindow "Game of Life" (windowWidth, windowHeight) (100, 100))
         (light (greyN 0.7))
         60
         initialWorld
         render
         handleInput
         update
