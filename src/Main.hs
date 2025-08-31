{-# LANGUAGE RecordWildCards #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), SpecialKey(..), KeyState(..), Modifiers(..))

-- Represent the world
data World = World
  { grid              :: [[Bool]]
  , running           :: Bool
  , tickInterval      :: Float
  , timeSinceLastTick :: Float
  , cellSize          :: Float
  }

-- Example grid: a blinker
initialGrid :: [[Bool]]
initialGrid =
  [ [False, True,  False]
  , [False, True,  False]
  , [False, True,  False]
  ]

initialWorld :: World
initialWorld = World
  { grid              = initialGrid
  , running           = False
  , tickInterval      = 0.5
  , timeSinceLastTick = 0
  , cellSize          = 40
  }

-- Render world as picture
render :: World -> Picture
render World{..} = Pictures $
  drawGrid grid cellSize ++
  [ translate (-280) 260 $
      scale 0.15 0.15 $
      color white $
      text (if running then "Running" else "Paused")
  ]

-- Draws the grid of cells
drawGrid :: [[Bool]] -> Float -> [Picture]
drawGrid g size =
  [ translate x y (color (cellColor alive) (rectangleSolid size size))
  | (row, j) <- zip g [0..]
  , (alive, i) <- zip row [0..]
  , let x = fromIntegral i * size - halfWidth
  , let y = fromIntegral (negate j) * size + halfHeight
  ]
  where
    rows = length g
    cols = length (head g)
    halfWidth  = fromIntegral cols * size / 2
    halfHeight = fromIntegral rows * size / 2

cellColor :: Bool -> Color
cellColor True  = white
cellColor False = greyN 0.3  -- lighter background for clarity

-- Handle keyboard/mouse input
handleInput :: Event -> World -> World
handleInput (EventKey (SpecialKey KeySpace) Down _ _) w =
  w { running = not (running w) }  -- toggle pause
handleInput _ w = w

-- Update world every frame
update :: Float -> World -> World
update dt w
  | running w =
      w { timeSinceLastTick = timeSinceLastTick w + dt }
  | otherwise = w

-- Main entry point
main :: IO ()
main = play
         (InWindow "Game of Life" (600, 600) (100, 100))  -- display
         black                                           -- background color
         60                                              -- steps per second
         initialWorld                                    -- initial world
         render                                          -- render function
         handleInput                                     -- input handler
         update                                          -- update function
