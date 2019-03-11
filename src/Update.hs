{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Update where

import Type
import Accessor
import Utils
import AI

-- | Function to update zombies
updateZombies :: Float  ->  [Plant] -> [Sunflower] -> [Zombie] -> [Zombie]
updateZombies dt ps sfs  = map (updateZombie dt ps sfs) . 
                        deleteZombie .
                        attackZombies  
  where
    attackZombies zs = map (reduceHealthZombie dt ps) zs  
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

updateSunflowers :: Float -> [Zombie]  ->  [Sunflower] -> [Sunflower]
updateSunflowers newTime zs  = map (sendSun newTime ) . 
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
    (x, y) = sCoords sf 
    newSun = Sun (x + 50, y - 30)
    oldSuns = sSun sf

plantShoots :: Float -> Float -> [Zombie] -> Plant -> Plant
plantShoots dt newTime zs p
  | not (any (peaVision (pCoords p)) zs) = p
  | (floor newTime) `mod` (6 :: Integer) == 0 = shoot 
  | otherwise = p { pBullet = moveProjectile (movedBullet bullet) }
  where
   shoot = p { pBullet = newBullet : bullet }
   newBullet = Projectile (x + 20, y)
   movedBullet b = map (moveBullet dt) b
   bullet = pBullet p
   (x, y) = pCoords p
   moveProjectile [] = []
   moveProjectile (pr:prs)
     | True `elem` collisions = moveProjectile prs
     | otherwise = pr : moveProjectile prs
     where
       zombiesCoords = map (zCoords) zs
       collisions = map (checkCollision (prCoords pr)) zombiesCoords

moveBullet :: Float -> Projectile -> Projectile
moveBullet dt projectile  = Projectile (x + dt*30, y)
  where
    (x, y) = prCoords projectile

       
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
reduceHealthZombie _dt [] z = z 
reduceHealthZombie dt (p:ps) z = reduce
  where 
    reduce 
      | True `elem` collisions = reduceHealthZombie dt ps newZombie 
      | otherwise = reduceHealthZombie dt ps z
      where 
        zXY = zCoords z
        collisions = map (checkCollision zXY) (prXYs (pBullet p))
        prXYs = map (prCoords) . map (moveBullet dt) 
        newZombie = z 
            {zDamage = (zDamage z) + (pStrength (pType p)) }

