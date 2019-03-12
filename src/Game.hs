module Game where

import Structure.Object
import Type
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Render
import Settings
import Utils
import Update
import Handle

-- | Function to render universe
drawUniverse :: Universe -> Picture
drawUniverse u = field
              <> drawObject drawZombie zs
              <> drawObject drawSunflower sfs
              <> drawObject drawPlant  ps
              <> drawObject drawCard cs 
              <> uScreen u
  where
    zs  = uEnemies u
    ps  = uDefense u
    sfs = uSunflowers u
    cs  = uCards u

-- | Function to change universe according
-- to its rules by the interaction with the player
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (MouseButton LeftButton) Down _ mouseCoords) u = 
  u { uCards = (updateCards mouseCoords (uCards u)) }
handleUniverse _  u = u

-- | Function to change universe according
-- to its rules by the time passed
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u
  | isWon u = u
         { uScreen = win }
  | isLost u = u
         { uScreen = lost }
  | otherwise = u
        { uEnemies = newEnemies
        , uDefense = newDefense
        , uSunflowers =  newSunflower
        , uTime    = newTime
        }
  where
    newEnemies = updateZombies dt u 
    newDefense = updatePlants dt u 
    newSunflower = updateSunflowers dt u 
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
