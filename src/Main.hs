{-# LANGUAGE RecordWildCards #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- Conway's Game of Life in Haskell using Gloss.
-- 
-- Features:
-- * Interactive drawing with mouse (click + drag to add/remove cells).
-- * Start/stop simulation with Space.
-- * Step through manually with Right arrow.
-- * Adjust speed with Up/Down arrows.
-- * Toggle wraparound edges with W.
-- * Randomize/reset colors with R / D.
--
-- Grid: 30x30, cell size 20px.
-- Background: light gray with helpful legend.

-- Represent the world
data World = World
  { grid              :: Grid
  , running           :: Bool
  , tickInterval      :: Float
  , timeSinceLastTick :: Float
  , colorRandNum      :: Float
  , aliveColor        :: Color
  , deadColor         :: Color
  , wrapAround        :: Bool
  , mouseDown         :: Bool
  , drawState         :: Maybe Bool
  }

-- Type alias for readability
type Grid = [[Bool]]

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
windowHeight = round (fromIntegral numRows * cellSize + 4 * padding)

-- Empty grid
emptyGrid :: Int -> Int -> Grid
emptyGrid rows cols = replicate rows (replicate cols False)

-- Example grid: blinker in the middle
initialGrid :: Grid
initialGrid =
  let rows = numRows
      cols = numCols
      base = emptyGrid rows cols
      midR = rows `div` 2
      midC = cols `div` 2
  in [ [ if (r, c) `elem` [(midR-1, midC), (midR, midC), (midR+1, midC)]
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
  , mouseDown         = False
  , drawState         = Nothing
  }

-- Render world
render :: World -> Picture
render w@World{..} = Pictures $
  drawGrid w ++
  [ translate (-fromIntegral windowWidth/2 + 20) (fromIntegral windowHeight/2 - 30) $
      scale 0.15 0.15 $
      color black $
      text (if running then "Running" else "Paused")
  , translate (fromIntegral windowWidth/2 - 200) (fromIntegral windowHeight/2 - 30) $
      scale 0.15 0.15 $
      color black $
      text ("Grid wrapping: " ++ if wrapAround then "On" else "Off")
  , translate (-fromIntegral windowWidth/2 + 20) (-fromIntegral windowHeight/2 + 90) $
      scale 0.12 0.12 $ color black $ text "Controls:"
  , translate (-fromIntegral windowWidth/2 + 40) (-fromIntegral windowHeight/2 + 70) $
      scale 0.10 0.10 $ color black $ text "Space - Start/Pause"
  , translate (-fromIntegral windowWidth/2 + 40) (-fromIntegral windowHeight/2 + 50) $
      scale 0.10 0.10 $ color black $ text "Right Arrow - Step once"
  , translate (-fromIntegral windowWidth/2 + 40) (-fromIntegral windowHeight/2 + 30) $
      scale 0.10 0.10 $ color black $ text "Up/Down Arrows - Speed +/-"
  , translate (0) (-fromIntegral windowHeight/2 + 70) $
      scale 0.10 0.10 $ color black $ text "Left Click + Drag - Draw/Erase cells"
  , translate (0) (-fromIntegral windowHeight/2 + 50) $
      scale 0.10 0.10 $ color black $ text "R - Random colors, D - Reset colors"
  , translate (0) (-fromIntegral windowHeight/2 + 30) $
      scale 0.10 0.10 $ color black $ text "W - Toggle wraparound"
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
    halfWidth  = fromIntegral (numCols - 1) * cellSize / 2   -- -1 for padding
    halfHeight = fromIntegral (numRows + 3) * cellSize / 2   -- +3 for padding

cellColor :: World -> Bool -> Color
cellColor World{..} True  = aliveColor
cellColor World{..} False = deadColor

-- Handle input
handleInput :: Event -> World -> World

-- Mouse down: decide whether we are drawing or erasing
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) w@World{..} =
  let (row, col) = mouseToCell x y
  in if inBounds row col
       then let current = grid !! row !! col
                newState = not current  -- toggle logic → if alive then we erase, else draw
            in w { grid = setCell row col newState grid
                 , mouseDown = True
                 , drawState = Just newState }
       else w

-- Mouse up: stop drawing
handleInput (EventKey (MouseButton LeftButton) Up _ _) w =
  w { mouseDown = False, drawState = Nothing }

-- Mouse drag: paint cells while mouse is down
handleInput (EventMotion (x, y)) w@World{..}
  | mouseDown =
      let (row, col) = mouseToCell x y
      in if inBounds row col
           then case drawState of
                  Just val -> w { grid = setCell row col val grid }
                  Nothing  -> w
           else w
  | otherwise = w

-- Toggle running
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

-- Convert mouse (x,y) to grid cell indices.
-- Note: we shift by +5 rows because the grid is drawn with extra vertical padding
--       to leave room for the legend and UI text.
mouseToCell :: Float -> Float -> (Int, Int)
mouseToCell x y =
  let col = floor ((x + fromIntegral windowWidth  / 2) / cellSize) - 1
      row = numRows + 5 - floor ((y + fromIntegral windowHeight / 2) / cellSize)
  in (row, col)

-- Bounds check
inBounds :: Int -> Int -> Bool
inBounds row col = row >= 0 && row < numRows && col >= 0 && col < numCols

-- Set cell to a specific value (not toggle)
setCell :: Int -> Int -> Bool -> Grid -> Grid
setCell row col val grid =
  [ [ if r == row && c == col then val else cell
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
nextGeneration :: Bool -> Grid -> Grid
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
  (light (greyN 0.75))
  60
  initialWorld
  render
  handleInput
  update
