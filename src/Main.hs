import Graphics.Gloss

-- Represent the world
data World = World
  { grid     :: [[Bool]]
  , cellSize :: Float
  }

-- Example grid: a vertical blinker
initialWorld :: World
initialWorld = World
  { grid = [ [False, True,  False]
           , [False, True,  True]
           , [False, True,  False]
           ]
  , cellSize = 50
  }

-- Convert World -> Picture
render :: World -> Picture
render (World g size) = Pictures $
  [ translate x y (color (cellColor alive) (rectangleSolid size size))
  | (row, j) <- zip g [0..]
  , (alive, i) <- zip row [0..]
  , let x = fromIntegral i * size - halfWidth
  , let y = fromIntegral (negate j) * size + halfHeight
  ]
  where
    rows = length g
    cols = length (head g)
    halfWidth  = fromIntegral (cols - 1) * size / 2
    halfHeight = fromIntegral (rows -1) * size / 2

cellColor :: Bool -> Color
cellColor True  = white
cellColor False = black

-- Main just displays the initial world
main :: IO ()
main = display
         (InWindow "Game of Life" (400, 200) (300, 300))
         (greyN 0.8)
         (render initialWorld)
