{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import Structure.Object
import Type
import Accessor
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap

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
  [ Plant PlantOne (-150,  100) 0 []
  , Plant PlantOne (-150,    0) 0 []
  , Plant PlantOne (-150, -100) 0 []
  ]
 
-- | Predefined sunflowers structure
sampleSunflowers :: [Sunflower]
sampleSunflowers = 
    [ Sunflower (-100, 100) 0 [] 
    ]

-- | Starter universe
initUniverse :: Universe
initUniverse = Universe 
               sampleZombies
               samplePlants
               sampleSunflowers
               False
               0
               blank

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
           <> projectiles
  where
    (x,   y) = pCoords p
    projectiles = drawObject (drawProjectile y) (pBullet p)
    pic = pPicture (pType p)

drawProjectile :: Float -> Projectile -> Picture
drawProjectile y p = Translate x y projectile
  where
    x = prX p
    
drawSunflower :: Sunflower -> Picture 
drawSunflower sf = Translate  x y pic
                <> sun
  where 
    (x,   y) = sCoords sf
    sun = drawObject drawSun (sSun sf)
    pic = color yellow (rectangleSolid 20 20)

drawSun :: Sun -> Picture
drawSun s = Translate x y pic
  where
    (x, y) = sunCoords s
    pic = color yellow (circleSolid 5)
    
-- | Function to render universe
drawUniverse :: Universe -> Picture
drawUniverse u = field
              <> drawObject drawZombie zs
              <> drawObject drawSunflower sf
              <> drawObject drawPlant  ps
              <> specialScreen u
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
updateUniverse dt u
  | isWon u = u
         { specialScreen = win }
  | isLost u = u
         { specialScreen = lost }
  | otherwise = u
        { uEnemies = newEnemies
        , uDefense = newDefense
        , uSunflowers =  newSunflower
        , uTime    = newTime
        }
  where
    newEnemies = updateZombies dt  (uDefense u) (uSunflowers u)   (uEnemies u)
    newDefense = updatePlants dt newTime (uEnemies u) (uDefense u)
    newSunflower = updateSunflowers dt newTime (uEnemies u) (uSunflowers  u) 
    newTime = (uTime u) + dt

isWon :: Universe -> Bool
isWon u = length z == 0
  where
    z = uEnemies u

isLost :: Universe -> Bool
isLost u = some
  where
    some = True `elem` sms
    z = uEnemies u
    zXs = map (fst . zCoords) z
    sms = map (\x -> x < -150) zXs

    
-- | Function to update zombies
updateZombies :: Float  ->  [Plant] -> [Sunflower] -> [Zombie] -> [Zombie]
updateZombies dt ps sfs  = map (updateZombie dt ps sfs) . 
                        deleteZombie .
                        attackZombies ps  
  where
    attackZombies ps zs = map (reduceHealthZombie dt ps) zs  
    deleteZombie zombies = filter (hasHealth) zombies
    hasHealth z = (zDamage z) <= (zHealth (zType z))                
                        

-- | Function to update one zombie
updateZombie :: Float -> [Plant] -> [Sunflower] -> Zombie -> Zombie
updateZombie dt ps sfs z
  | True `elem` collisions = z
  | otherwise              = moveZombie dt z
  where
    plantsCoords = map (pCoords) ps
    sunflowersCoords = map (sCoords) sfs
    collisions = map (checkCollision (zCoords z)) plantsCoords ++ sCollisions
    sCollisions = map (checkCollision (zCoords z)) sunflowersCoords 



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

    
updateSunflowers :: Float  -> Float -> [Zombie]  ->  [Sunflower] -> [Sunflower]
updateSunflowers dt newTime zs  = map (sendSun newTime ) . 
                          deleteSunflower .
                          attackSunflowers newTime zs  
  where 
    deleteSunflower sunflowers = filter (hasHealth) sunflowers
    hasHealth s = (sDamage s) <= (sHealth  s)
                        
sendSun :: Float -> Sunflower -> Sunflower
sendSun newTime sf
  | ((round newTime) `mod` (13 :: Integer) == 0) = send
  | otherwise = sf
  where 
    send = sf {sSun = newSun : oldSuns} 
    newSun = Sun (-75, 75)
    oldSuns = sSun sf
    
plantShoots :: Float -> Float -> [Zombie] -> Plant -> Plant
plantShoots dt newTime zs p
  | (round newTime) `mod` (6 :: Integer) == 0 = shoot
  | otherwise = p { pBullet = moveProjectile movedBullet }
  where
   shoot = p { pBullet = newBullet : bullet }
   newBullet = Projectile (-135)
   movedBullet = map (moveBullet dt) bullet
   bullet = pBullet p
   px = map prX bullet
   (_x, y) = pCoords p
   moveProjectile [] = []
   moveProjectile (pr:prs)
     | True `elem` collisions = moveProjectile prs
     | otherwise = pr : moveProjectile prs
     where
       zombiesCoords = map (zCoords) zs
       collisions = map (checkCollision (prX pr, y)) zombiesCoords
 
moveBullet :: Float -> Projectile -> Projectile
moveBullet dt projectile  = Projectile (px + dt*30)
  where
    px = prX projectile

       
-- | Function to lower health of plants
attackPlants :: Float -> [Zombie] -> [Plant] -> [Plant]
attackPlants dt zs ps 
  | (floor dt) `mod` (6 :: Integer) == 0 = map (reduceHealthPlant zs) ps
  | otherwise = ps


attackSunflowers ::  Float -> [Zombie] -> [Sunflower] -> [Sunflower]
attackSunflowers dt zs sfs  
  | (floor dt) `mod` (6 :: Integer) == 0 = map (reduceHealthSunflower zs) sfs
  | otherwise = sfs 
  

reduceHealthSunflower :: [Zombie] -> Sunflower -> Sunflower
reduceHealthSunflower  [] s = s 
reduceHealthSunflower (z:zs) s = reduce
  where
    reduce
      | checkCollision zXY sXY = reduceHealthSunflower zs newSunflower
      | otherwise = reduceHealthSunflower zs s
      where
        zXY = zCoords z
        sXY = sCoords s
        newSunflower = s
           { sDamage = (sDamage s) + (zStrength (zType z)) }
   
  
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
      | True `elem` collisions = reduceHealthZombie dt ps newZombie 
      | otherwise = reduceHealthZombie dt ps z
      where 
        zXY = zCoords z
        collisions = map (checkCollision (zCoords z)) prCoords
        prCoords = zip (repeat y) (prXs (pBullet p))
        prXs = map (prX) . map (moveBullet dt) 
        (_x, y) = pCoords p 
        newZombie = z 
            {zDamage = (zDamage z) + (pStrength (pType p)) }

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
