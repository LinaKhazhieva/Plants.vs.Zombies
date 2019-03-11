{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import Structure.Object
import Type
import Accessor
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- | Predefined wave of enemies
sampleZombies :: [Zombie]
sampleZombies = 
  [ Zombie ZombieOne (150,  100) 0 
  , Zombie ZombieTwo (150,    0) 0 
  , Zombie ZombieThree (150, -100) 0
  ]

-- | Predefined defense structure
samplePlants :: [Plant]
samplePlants =
  [ Plant PlantOne (-150,  100) 0 (Projectile (-400))
  , Plant PlantOne (-150,    0) 0 (Projectile (-400))
  , Plant PlantOne (-150, -100) 0 (Projectile (-400))
  ]
 
-- | Predefined sunflowers structure
sampleSunflowers :: [Sunflower]
sampleSunflowers = 
    [ Sunflower (-100, 100) 0 (Sun (-75, 75))
    ]

-- | Starter universe
initUniverse :: Universe
initUniverse = Universe 
               sampleZombies
               samplePlants
               sampleSunflowers
               0

-- | High-level function to draw an object
-- of the game on the screen
drawObject :: (a -> Picture) -> [a] -> Picture
drawObject draw xs = pictures (map draw xs)

-- | Function to render zombie on the
-- screen
drawZombie :: Zombie -> Picture
drawZombie z = Translate x y pic 
  where
    (x, y) = zCoords z
    pic = zPicture (zType z)

-- | Function to render plant on the
-- screen
drawPlant :: Plant -> Picture
drawPlant p = Translate  x y pic
           <> Translate px y projectile
  where
    (x,   y) = pCoords p
    px = prX (pBullet p)
    pic = pPicture (pType p)
    


drawSunflower :: Sunflower -> Picture 
drawSunflower sf = Translate  x y pic
                <> Translate sx sy sun
  where 
    (x,   y) = sCoords sf
    (sx, sy) =  sunCoords (sSun sf)
    sun = color yellow (circleSolid 5)
    pic = color yellow (rectangleSolid 20 20)
    
drawSun :: Sun -> Picture 
drawSun s = Translate  x y pic
  where 
    (x, y) = sunCoords s
    pic = color yellow (circleSolid 5)


-- | Function to render universe
drawUniverse :: Universe -> Picture
drawUniverse u = drawObject drawZombie zs
              <> drawObject drawSunflower sf
              <> drawObject drawPlant  ps
              <> field
  where
    zs = uEnemies u
    ps = uDefense u
    sf = uSunflowers u

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
--  , uSunflower =  newSunflower
  , uTime    = newTime
  }
  where
    newEnemies = updateZombies dt (uDefense u) (uEnemies u)
    newDefense = updatePlants dt newTime (uEnemies u) (uDefense u)
--    newSunflower = updateSunflower dt newTime (uEnemies u) (uDefense u)
    newTime = (uTime u) + dt

-- | Function to update zombies
updateZombies :: Float  ->  [Plant] -> [Zombie] -> [Zombie]
updateZombies dt ps  = map (updateZombie dt ps) . 
                        deleteZombie .
                        attackZombies ps  
  where
    attackZombies ps zs = map (reduceHealthZombie dt ps) zs  
    deleteZombie zombies = filter (hasHealth) zombies
    hasHealth z = (zDamage z) <= (zHealth (zType z))                
                        

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
    v      = zSpeed  (zType z)

-- | Function to update plant
updatePlants :: Float -> Float -> [Zombie] -> [Plant] -> [Plant]
updatePlants dt newTime zs = map (plantShoots dt newTime zs) .
                             deletePlant . 
                             attackPlants newTime zs                     
  where
    deletePlant plants = filter (hasHealth) plants
    hasHealth p = (pDamage p) <= (pHealth (pType p))



plantShoots :: Float -> Float -> [Zombie] -> Plant -> Plant
plantShoots dt newTime zs p
  | (round newTime) `mod` (13 :: Integer) == 0 = shoot
  | otherwise = moveProjectile
  where
   shoot = p { pBullet = newBullet }
   newBullet = bullet { prX = -135 }
   movedBullet = bullet { prX = px + dt*30 }
   bullet = pBullet p
   px = prX bullet
   (_x, y) = pCoords p
   moveProjectile
     | True `elem` collisions = p { pBullet = Projectile (-400) }
     | otherwise = p { pBullet = movedBullet }
     where
       zombiesCoords = map (zCoords) zs
       collisions = map (checkCollision (prX movedBullet, y)) zombiesCoords
 
moveBullet :: Float -> Float -> Projectile
moveBullet dt px  = Projectile (  px + dt*30 ) 

       
-- | Function to lower health of plants
attackPlants :: Float -> [Zombie] -> [Plant] -> [Plant]
attackPlants dt zs ps 
  | (floor dt) `mod` (6 :: Integer) == 0 = map (reduceHealthPlant zs) ps
  | otherwise = ps

-- | Function to reduce health of plant
reduceHealthPlant :: [Zombie] -> Plant -> Plant
reduceHealthPlant [] p = p
reduceHealthPlant (z:zs) p = reduce
  where
    reduce
      | checkCollision zXY pXY = reduceHealthPlant zs newPlant
      | otherwise = reduceHealthPlant zs p
      where
        zXY = zCoords z
        pXY = pCoords p
        newPlant = p
           { pDamage = (pDamage p) + (zStrength (zType z)) }

-- | Function to reduce health of zombie 
reduceHealthZombie :: Float ->  [Plant] -> Zombie -> Zombie
reduceHealthZombie dt [] z = z 
reduceHealthZombie dt (p:ps) z = reduce
  where 
    reduce 
      | checkCollision zXY prXY = reduceHealthZombie dt ps newZombie 
      | otherwise = reduceHealthZombie dt ps z
      where 
        zXY = zCoords z
        prXY = (prX (moveBullet dt (prX  (pBullet p))), y )
        (_x, y) = pCoords p 
        newZombie = z 
            {zDamage = (zDamage z) + (pHealth (pType p)) }

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
