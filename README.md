# Conway's Game of Life - Haskell Implementation

A fully interactive implementation of Conway's Game of Life using Haskell and the Gloss graphics library.

## Table of Contents
- [User Guide](#user-guide)
- [Programmer Documentation](#programmer-documentation)

---

## User Guide

### What is Conway's Game of Life?

Conway's Game of Life is a cellular automaton devised by mathematician John Conway. It's a zero-player game where the evolution is determined by its initial state, requiring no further input. The "game" consists of a grid of cells that can be either alive or dead, and they evolve according to simple rules.

### Getting Started

When you run the program, you'll see a 30x30 grid with a simple "blinker" pattern (three cells in a vertical line) in the center. The simulation starts paused, allowing you to modify the initial configuration.

### Controls

#### Mouse Controls
- **Left Click + Drag**: Draw or erase cells
  - Click on an empty (dead) cell to make it alive
  - Click on a living cell to kill it
  - Drag to paint multiple cells at once

#### Keyboard Controls
- **Space**: Start/pause the simulation
- **Right Arrow**: Step through one generation manually (useful for detailed observation)
- **Up Arrow**: Increase simulation speed
- **Down Arrow**: Decrease simulation speed
- **W**: Toggle wraparound mode (cells at edges connect to opposite sides)
- **R**: Randomize colors for alive and dead cells
- **D**: Reset to default colors (white alive, black dead)

### Game Rules

Each cell follows these simple rules each generation:

1. **Survival**: A living cell with 2 or 3 living neighbors survives
2. **Death by isolation**: A living cell with fewer than 2 neighbors dies
3. **Death by overcrowding**: A living cell with more than 3 neighbors dies
4. **Birth**: A dead cell with exactly 3 living neighbors becomes alive

### Interface Elements

The window displays:
- Current simulation status (Running/Paused)
- Wraparound mode status (On/Off)
- Complete control reference at the bottom

### Tips for Interesting Patterns

Try creating these classic patterns:

- **Blinker**: Three cells in a line (already provided as default)
- **Block**: 2x2 square of cells (static pattern)
- **Glider**: A pattern that moves across the grid
- **Beacon**: Four cells in an L-shape that oscillates

---

## Programmer Documentation

### Architecture Overview

The program uses functional reactive programming principles with the Gloss library for rendering and event handling. The core architecture consists of:

- **State Management**: Pure functional state updates
- **Rendering**: Declarative picture composition
- **Event Handling**: Pattern matching on input events
- **Game Logic**: Pure functions for cellular automaton rules

### Data Structures

#### World Record
```haskell
data World = World
  { grid              :: Grid           -- Current cell states
  , running           :: Bool           -- Simulation state
  , tickInterval      :: Float          -- Time between generations
  , timeSinceLastTick :: Float          -- Timing accumulator
  , colorRandNum      :: Float          -- Random seed for colors
  , aliveColor        :: Color          -- Color for living cells
  , deadColor         :: Color          -- Color for dead cells
  , wrapAround        :: Bool           -- Edge behavior
  , mouseDown         :: Bool           -- Mouse interaction state
  , drawState         :: Maybe Bool     -- Drawing mode (add/remove)
  }
```

#### Grid Type
```haskell
type Grid = [[Bool]]  -- 2D list representing cell states
```

### Key Constants

- **Grid Size**: 30x30 cells
- **Cell Size**: 20 pixels per cell
- **Window Padding**: 40 pixels around grid
- **Default Tick Interval**: 0.3 seconds
- **Frame Rate**: 60 FPS

### Core Functions

#### Game Logic

**`nextGeneration :: Bool -> Grid -> Grid`**
- Computes the next state of all cells
- Takes wraparound mode as parameter
- Applies Conway's rules to each cell based on neighbor count

**`liveNeighbors :: Int -> Int -> Int`** (local function)
- Counts living neighbors for a given cell
- Handles both wraparound and bounded grid modes
- Excludes the cell itself from the count

**`applyRules :: Bool -> Int -> Bool`** (local function)
- Implements Conway's four rules
- Takes current cell state and neighbor count
- Returns new cell state

#### Rendering

**`render :: World -> Picture`**
- Main rendering function
- Combines grid visualization with UI text
- Uses Gloss's declarative picture composition

**`drawGrid :: World -> [Picture]`**
- Converts grid state to visual rectangles
- Handles coordinate transformation from grid to screen space
- Applies current color scheme

**`cellColor :: World -> Bool -> Color`**
- Maps cell state to display color
- Uses current alive/dead color settings

#### Input Handling

**`handleInput :: Event -> World -> World`**
- Pattern matches on all input events
- Handles mouse interaction for cell editing
- Processes keyboard shortcuts for simulation control

**`mouseToCell :: Float -> Float -> (Int, Int)`**
- Converts screen coordinates to grid indices
- Accounts for window padding and coordinate system differences

#### Utility Functions

**`setCell :: Int -> Int -> Bool -> Grid -> Grid`**
- Immutably updates a single cell in the grid
- Returns new grid with modified cell

**`randomColor :: StdGen -> Color`**
- Generates random RGB colors
- Uses System.Random for deterministic randomness

**`emptyGrid :: Int -> Int -> Grid`**
- Creates a grid with all cells dead
- Used for initialization

### Coordinate Systems

The program handles three coordinate systems:

1. **Screen Coordinates**: Gloss window coordinates (origin at center)
2. **Grid Coordinates**: Array indices (0,0 at top-left)
3. **Visual Coordinates**: Translated for proper display positioning

### Performance Considerations

- **Grid Size**: Limited to 30x30 for smooth performance
- **Pure Functions**: All state updates are immutable
- **Lazy Evaluation**: Haskell's laziness helps with grid computations
- **Frame Rate**: 60 FPS ensures smooth animation

### Extension Points

The code is designed for easy modification:

1. **Grid Size**: Change `numRows` and `numCols` constants
2. **Colors**: Modify color generation or add color schemes
3. **Rules**: Alter `applyRules` for different cellular automata
4. **Patterns**: Add preset pattern generators
5. **UI**: Extend the text display or add new controls

### Dependencies

- **Graphics.Gloss**: Main graphics and game loop library
- **Graphics.Gloss.Interface.Pure.Game**: Event handling
- **System.Random**: Random number generation

### Building and Running

```bash
ghc -O2 GameOfLife.hs
./GameOfLife
```

Or with cabal/stack:
```bash
stack exec ghc -- -O2 GameOfLife.hs
```

### Code Style Notes

- Uses `RecordWildCards` language extension for clean record access
- Follows functional programming principles with pure functions
- Pattern matching for clear control flow
- Local `where` clauses for helper functions
- Type signatures for documentation and safety

### Potential Improvements

1. **Configuration File**: External settings for colors, speed, patterns
2. **Pattern Library**: Built-in collection of famous Game of Life patterns
3. **Zoom Feature**: Ability to zoom in/out of the grid
4. **Save/Load**: Persistence of interesting configurations
5. **Statistics**: Population tracking, generation counter
6. **Variable Grid**: Runtime-configurable grid dimensions