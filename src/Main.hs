{-# LANGUAGE RecordWildCards #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- Represent the world
data World = World
  { grid              :: [[Bool]]
  , running           :: Bool
  , tickInterval      :: Float
  , timeSinceLastTick :: Float
  , colorRandNum      :: Float
  , aliveColor        :: Color
  , deadColor         :: Color
  , wrapAround        :: Bool
  }

-- Grid dimensions
numRows, numCols :: Int
numRows = 30
numCols = 30

-- Cell size
cellSize :: Float
cellSize = 20

-- Cell padding
padding :: Float
padding = 40  -- space around grid for text and nicer look

-- Window dimensions
windowWidth, windowHeight :: Int
windowWidth  = round (fromIntegral numCols * cellSize + 1 * padding)
windowHeight = round (fromIntegral numRows * cellSize + 2 * padding)

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

-- Initial world
initialWorld :: World
initialWorld = World
  { grid              = initialGrid
  , running           = False
  , tickInterval      = 0.3
  , timeSinceLastTick = 0
  , colorRandNum      = 0 
  , aliveColor        = white
  , deadColor         = black
  , wrapAround        = False
  }

-- Render world
render :: World -> Picture
render w@World{..} = Pictures $
  drawGrid w ++
  [ translate (-fromIntegral windowWidth/2 + 20) (fromIntegral windowHeight/2 - 30) $
      scale 0.15 0.15 $
      color black $
      text (if running then "Running" else "Paused")
  , translate (fromIntegral windowWidth/2 - 100) (fromIntegral windowHeight/2 - 30) $
      scale 0.12 0.12 $
      color black $
      text ("Wrap: " ++ if wrapAround then "On" else "Off")
  ]

-- Draw grid
drawGrid :: World -> [Picture]
drawGrid w@World{..} =
  [ translate x y (color (cellColor w cell) (rectangleSolid cellSize cellSize))
  | (row, r) <- zip grid ([0..] :: [Int])
  , (cell, c) <- zip row ([0..] :: [Int])
  , let x = fromIntegral c * cellSize - halfWidth
  , let y = fromIntegral (negate r) * cellSize + halfHeight
  ]
  where
    halfWidth  = fromIntegral (numCols - 1) * cellSize / 2
    halfHeight = fromIntegral (numRows - 1) * cellSize / 2

cellColor :: World -> Bool -> Color
cellColor World{..} True  = aliveColor
cellColor World{..} False = deadColor

-- Handle input
handleInput :: Event -> World -> World
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) w@World{..} =
  let col = floor ((x + fromIntegral windowWidth  / 2) / cellSize) - 1
      row = numRows + 1 - floor ((y + fromIntegral windowHeight / 2) / cellSize)
  in if row >= 0 && row < numRows && col >= 0 && col < numCols
       then w { grid = toggleCell row col grid }
       else w

handleInput (EventKey (SpecialKey KeySpace) Down _ _) w =
  w { running = not (running w) }

-- Advance one generation manually
handleInput (EventKey (SpecialKey KeyRight) Down _ _) w =
  w { grid = nextGeneration (wrapAround w) (grid w) }

-- Speed up simulation
handleInput (EventKey (SpecialKey KeyUp) Down _ _) w =
  w { tickInterval = max 0.02 (tickInterval w * 0.8) }

-- Slow down simulation
handleInput (EventKey (SpecialKey KeyDown) Down _ _) w =
  w { tickInterval = tickInterval w * 1.25 }

-- Random color palette
handleInput (EventKey (Char 'r') Down _ _) w =
  let g1 = mkStdGen (floor $ 1000 * colorRandNum w)
      g2 = mkStdGen (floor $ 3000 * colorRandNum w)
  in w { aliveColor = randomColor g1
       , deadColor  = randomColor g2 }

-- Default colors
handleInput (EventKey (Char 'd') Down _ _) w =
  w { aliveColor = white, deadColor = black }

-- toggle wrapping
handleInput (EventKey (Char 'w') Down _ _) w =
  w { wrapAround = not (wrapAround w) }

handleInput _ w = w

toggleCell :: Int -> Int -> [[Bool]] -> [[Bool]]
toggleCell row col grid =
  [ [ if r == row && c == col then not cell else cell
    | (c, cell) <- zip [0..] rowVals ]
  | (r, rowVals) <- zip [0..] grid ]

-- Update world
update :: Float -> World -> World
update dt w
  | running w && timeSinceLastTick w + dt >= tickInterval w =
      w { grid = nextGeneration (wrapAround w) (grid w), timeSinceLastTick = 0, colorRandNum = colorRandNum w + dt }
  | otherwise = w { timeSinceLastTick = timeSinceLastTick w + dt, colorRandNum = colorRandNum w + dt }
  -- | otherwise = w

-- Compute next generation
nextGeneration :: Bool -> [[Bool]] -> [[Bool]]
nextGeneration wrap g =
  [ [ applyRules (g !! r !! c) (liveNeighbors r c)
    | c <- [0..cols-1] ]
  | r <- [0..rows-1] ]
  where
    rows = length g
    cols = if null g then 0 else length (head g)

    liveNeighbors r c =
      length [ () 
             | dr <- [-1..1], dc <- [-1..1]
             , not (dr == 0 && dc == 0)
             , let r' = if wrap then (r + dr) `mod` rows else r + dr
             , let c' = if wrap then (c + dc) `mod` cols else c + dc
             , r' >= 0, r' < rows
             , c' >= 0, c' < cols
             , g !! r' !! c' ]

    applyRules alive n
      | alive && (n == 2 || n == 3) = True
      | not alive && n == 3         = True
      | otherwise                   = False

-- Generate random color
randomColor :: StdGen -> Color
randomColor gen =
  let (r, gen1) = randomR (0.0, 1.0) gen
      (g, gen2) = randomR (0.0, 1.0) gen1
      (b, _)  = randomR (0.0, 1.0) gen2
  in makeColor r g b 1.0

-- Main
main :: IO ()
main = play
  (InWindow "Game of Life" (windowWidth, windowHeight) (300, 100))
  (light (greyN 0.7))
  60
  initialWorld
  render
  handleInput
  update
