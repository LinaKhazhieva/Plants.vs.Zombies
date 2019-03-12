{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Type
import Settings

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
    size   = boxSize

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
