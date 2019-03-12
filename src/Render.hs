{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Render where

import Graphics.Gloss
import Type
import Settings

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
drawPlant p = Translate x y pic
  where
    (x,   y) = pCoords p
    pic = pPicture (pType p)

drawProjectile :: Projectile -> Picture
drawProjectile pr = Translate x (y + deltaYProjectile) pic
  where
    (x, y) = prCoords pr
    pic    = prPicture (prType pr)

drawProjectiles :: ProjectileType -> [Projectile] -> Picture
drawProjectiles t prs = drawObject drawProjectile newPrs
  where
    newPrs = filter (\pr -> (prType pr) == t) prs

drawCard :: Card -> Picture
drawCard card = translate x y
                (pictures [if (isActive card) then drawLining else blank, 
                color (cardColor card) (rectangleSolid cardWidth cardHeight)])
  where
    drawLining = color cardLiningColor 
      (rectangleSolid (cardWidth + cardLiningThickness * 2) (cardHeight + cardLiningThickness * 2))
    (x, y) = cardCoords card
