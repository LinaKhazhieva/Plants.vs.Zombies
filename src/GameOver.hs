{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module GameOver where

import Type
import Settings

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
