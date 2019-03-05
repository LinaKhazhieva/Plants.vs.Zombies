{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import Structure.Object
import Type
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- | Predefined wave of enemies
sampleZombies :: [Zombie]
sampleZombies = 
  [ Zombie (150,  100) 4 1
  , Zombie (150,    0) 5 1
  , Zombie (150, -100) 7 1
  ]

-- | Predefined defense structure
samplePlants :: [Plant]
samplePlants =
  [ Plant (-150,  100) 10 (Projectile (-210))
  , Plant (-150,    0) 10 (Projectile (-210))
  , Plant (-150, -100) 10 (Projectile (-210))
  ]

-- | Starter universe
initUniverse :: Universe
initUniverse = Universe 
               sampleZombies
               samplePlants
               0

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
drawPlant p = Translate  x y  plant
           <> Translate px y projectile
  where
    (x,   y) = pCoords p
    px = prX (pBullet p)

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
  , uDefense = newDefense
  , uTime    = newTime
  }
  where
    newEnemies = updateZombies dt (uDefense u) (uEnemies u)
    newDefense = updatePlants dt newTime (uEnemies u) (uDefense u)
    newTime = (uTime u) + dt

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

-- | Function to update plant
updatePlants :: Float -> Float -> [Zombie] -> [Plant] -> [Plant]
updatePlants dt newTime zs = map (plantShoots dt newTime zs) .
                             deletePlant . 
                             attackPlants newTime zs                     
  where
    deletePlant plants = filter (hasHealth) plants
    hasHealth p = (pHealth p) > 0

plantShoots :: Float -> Float -> [Zombie] -> Plant -> Plant
plantShoots dt newTime zs p
  | (round newTime) `mod` (13 :: Integer) == 0 = shoot
  | otherwise = moveProjectile
  where
   shoot = p { pBullet = newBullet }
   newBullet = bullet { prX = -135 }
   movedBullet = bullet { prX = px + dt*20 }
   bullet = pBullet p
   px = prX bullet
   (_x, y) = pCoords p
   moveProjectile
     | True `elem` collisions = p { pBullet = Projectile (-210) }
     | otherwise = p { pBullet = movedBullet }
     where
       zombiesCoords = map (zCoords) zs
       collisions = map (checkCollision (prX movedBullet, y)) zombiesCoords

-- | Function to lower health of plants
attackPlants :: Float -> [Zombie] -> [Plant] -> [Plant]
attackPlants dt zs ps 
  | (floor dt) `mod` (6 :: Integer) == 0 = map (reduceHealth zs) ps
  | otherwise = ps

-- | Function to reduce health of plant
reduceHealth :: [Zombie] -> Plant -> Plant
reduceHealth [] p = p
reduceHealth (z:zs) p = reduce
  where
    reduce
      | checkCollision zXY pXY = reduceHealth zs newPlant
      | otherwise = reduceHealth zs p
      where
        zXY = zCoords z
        pXY = pCoords p
        newPlant = p
           { pHealth = (pHealth p) - (zStrength z) }

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
