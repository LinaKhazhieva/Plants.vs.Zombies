{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import Structure.Object
import Type
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Render
import Settings
import GameOver
import Update


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

perform :: IO()
perform = play
          screen
          white
          6
          initUniverse
          drawUniverse
          handleUniverse
          updateUniverse
