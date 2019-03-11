{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Render where

import Graphics.Gloss
import Type
import Structure.Object
import Accessor

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
