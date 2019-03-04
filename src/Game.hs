{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import Structure.Object
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- | Type for coordinates on the field
type Coords = (Float, Float)

-- | Type for rectangle, which used for
-- hit box of the elements
type Rectangle = (Coords, Coords)

-- | Data type for Zombie
data Zombie = Zombie
  { zCoords ::    Coords      -- ^ coordinates of zombie
  , zSpeed  ::     Float      -- ^ movement speed
  }

-- | Data type for Plants
data Plant = Plant
  { pCoords :: Coords      -- ^ coordinated of plants
  }

-- | Data type for whole Universe
data Universe = Universe
  { uEnemies :: [Zombie]   -- ^ list of enemies
  , uDefense ::  [Plant]   -- ^ list of plants
  }

-- | Predefined wave of enemies
sampleZombies :: [Zombie]
sampleZombies = 
  [ Zombie (150,  100) 4
  , Zombie (150,    0) 5
  , Zombie (150, -100) 7
  ]

-- | Predefined defense structure
samplePlants :: [Plant]
samplePlants =
  [ Plant (-150,  100)
  , Plant (-150,    0)
  , Plant (-150, -100)
  ]

-- | Starter universe
initUniverse :: Universe
initUniverse = Universe 
               sampleZombies
               samplePlants

-- | High-level function to draw an object
-- of the game on the screen
drawObject :: (a -> Picture) -> [a] -> Picture
drawObject draw xs = pictures (map draw xs)

-- | Function to render zombie on the
-- screen
drawZombie :: Zombie -> Picture
drawZombie z = Translate x y zombie
  where
    (x, y) = zCoords z

-- | Function to render plant on the
-- screen
drawPlant :: Plant -> Picture
drawPlant p = Translate x y plant
  where
    (x, y) = pCoords p

-- | Function to render universe
drawUniverse :: Universe -> Picture
drawUniverse u = drawObject drawZombie zs
              <> drawObject drawPlant  ps
              <> field
  where
    zs = uEnemies u
    ps = uDefense u

-- | Function to change universe according
-- to its rules by the interaction with the player
handleUniverse :: Event -> Universe -> Universe
handleUniverse _e u = u

-- | Function to change universe according
-- to its rules by the time passed
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u = u
  { uEnemies = newEnemies
  }
  where
    newEnemies = updateZombies dt (uDefense u) (uEnemies u)

-- | Function to update zombies
updateZombies :: Float -> [Plant] -> [Zombie] -> [Zombie]
updateZombies dt ps zs = map (updateZombie dt ps) zs

-- | Function to update one zombie
updateZombie :: Float -> [Plant] -> Zombie -> Zombie
updateZombie dt ps z
  | True `elem` collisions = z
  | otherwise              = moveZombie dt z
  where
    plantsCoords = map (pCoords) ps
    collisions = map (checkCollision (zCoords z)) plantsCoords

-- | Function to move zombies
moveZombie :: Float -> Zombie -> Zombie
moveZombie dt z = z
  { zCoords = (x - dt * v, y)
  }
  where
    (x, y) = zCoords z
    v      = zSpeed  z

-- | Function to check collisions of hitboxes
checkCollision :: Coords -> Coords -> Bool
checkCollision (x1, y1) (x2, y2) 
  | realX1 < realX2 + size &&
    realX1 + size > realX2 &&
    realY1 < realY2 + size  &&
    size / 20 + realY1 > realY2 = True
  | otherwise = False
  where
    realX1 = x1 - (size / 2)
    realY1 = y1 - (size / 2)
    realX2 = x2 - (size / 2)
    realY2 = y2 - (size / 2)
    size   = 25

perform :: IO()
perform = play
          screen
          white
          6
          initUniverse
          drawUniverse
          handleUniverse
          updateUniverse
