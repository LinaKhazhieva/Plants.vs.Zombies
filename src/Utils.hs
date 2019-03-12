{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Type
import Settings

-- | Function to check collisions of hitboxes
checkCollision
  :: Float  -- ^ width of the fst obj to determine hitbox
  -> Float  -- ^ heigth of the fst obj to determine hitbox
  -> Float  -- ^ width of the snd obj to determine hitbox
  -> Float  -- ^ height of the snd obj to determine hitbox
  -> Coords -- ^ coords of the fst obj
  -> Coords -- ^ coords of the snd obj
  -> Bool
checkCollision w1 h1 w2 h2 (x1, y1) (x2, y2)
  | realX1 < realX2 + w2 &&
    realX1 + w1 > realX2 &&
    realY1 < realY2 + h2  &&
    h1 + realY1 > realY2 = True
  | otherwise = False
  where
    realX1 = x1 - (w1 / 2)
    realY1 = y1 - (h1 / 2)
    realX2 = x2 - (w2 / 2)
    realY2 = y2 - (h2 / 2)

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
    sms = map (\x -> x < begginnerCoords) zXs
