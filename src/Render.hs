{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Render where

import           Graphics.Gloss
import           Settings
import           Structure.Alphabet (numPicture)
import           Structure.Object
import           Type

-- | High-level function to draw an object
-- of the game on the screen
drawObject :: (a -> Picture) -> [a] -> Picture
drawObject draw xs = pictures (map draw xs)

-- | Function to render zombie on the
--   screen. Draws pic associated with
--   corresponding type of zombie on
--   its coords.
drawZombie :: Zombie -> Picture
drawZombie z = Translate x y pic
  where
    (x, y) = zCoords z
    pic = zPicture (zType z)

-- | Function to render plant on the
--   screen. Draws pic associated with
--   corresponding type of plant on its
--   coords
drawPlant :: Plant -> Picture
drawPlant p = Translate x y pic
  where
    (x,   y) = pCoords p
    pic = pPicture (pType p)

-- | Function to render projectile of the
--   plants on screen. Draws pic associated
--   with correponsing projectile type
--   (either sun or pea), on its coords, with
--   y-coords deltad
drawProjectile :: Projectile -> Picture
drawProjectile pr = Translate x (y + deltaYProjectile) pic
  where
    (x, y) = prCoords pr
    pic    = prPicture (prType pr)

-- | Function to render projectiles of the corresponding
--   type.
--   Filters projectiles by its type and calls drawProjectile
--   for each projectile
drawProjectiles :: ProjectileType -> [Projectile] -> Picture
drawProjectiles t prs = drawObject drawProjectile newPrs
  where
    newPrs = filter (\pr -> (prType pr) == t) prs

-- | Function to render card on the screen. Cards can be drawn
--   in different ways.
-- * if player has not enough money to buy plant or plant is
--   cannot be bought - draw pic of correponding card with
--   additional transparent layer, which makes it unavailable
-- * otherwise - draw pic of corresponding card
drawCard :: Int -> Card -> Picture
drawCard m card
  | cond      = pic <> color transparentBlack rctngl
  | otherwise = pic
  where
    cond       = m < cMoney (plantType card) || cTime card > 0
    rctngl     = Translate x y (rectangleSolid cardWidth cardHeight)
    pic        = Translate x y (border <> cPicture (plantType card))
    border     = if (isActive card)
                 then drawLining
                 else blank
    drawLining = color cardLiningColor (rectangleSolid
                 (cardWidth + cardLiningThickness * 2)
                 (cardHeight + cardLiningThickness * 2))
    (x, y)     = cCoords card

drawMoney :: Int -> Picture
drawMoney m = Translate (-650) (227) pic
  where
    pic = numPicture m

renderMenuButton :: Picture
renderMenuButton = Translate 600 260 menuButton
