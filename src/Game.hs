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
              <> drawObject drawPlant  ps
              <> drawProjectiles Sun prs
              <> drawProjectiles Pea prs
              <> drawObject drawCard cs 
              <> uScreen u
  where
    prs = concat (map pBullet ps)
    zs  = uEnemies u
    ps  = uDefense u  
    cs  = uCards u

-- | Function to change universe according
--   to its rules by the interaction with the player
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (MouseButton LeftButton) Down _ mouseCoords) u = 
  u { uCards = (updateCards mouseCoords (uCards u)) }
handleUniverse _  u = u

-- | Function to change universe according
--   to its rules by the time passed
--   detect if the game is over
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u
  | isWon u = u
         { uScreen = win }
  | isLost u = u
         { uScreen = lost }
  | otherwise = u
        { uEnemies = newEnemies
        , uDefense = newDefense
        , uTime    = newTime
        }
  where
    newEnemies = updateZombies dt u 
    newDefense = updatePlants dt u
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
