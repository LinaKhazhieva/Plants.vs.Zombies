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
drawState :: State -> Picture
drawState (State u us) = field
                         <> drawObject drawPlant  ps
                         <> drawProjectiles Sun (prs ++ ss)
                         <> drawProjectiles Pea prs
                         <> drawObject drawZombie zs
                         <> drawObject (drawCard m) cs
                         <> drawMoney m 
                         <> uScreen u
  where
    prs      = concat (map pBullet ps)
    zs       = uEnemies u
    ps       = uDefense u  
    cs       = uCards u
    m        = uMoney u
    (ss, _)  = uSuns u

-- | Function to change universe according
--   to its rules by the interaction with the player
handleState :: Event -> State -> State
handleState (EventKey (MouseButton LeftButton)
               Down _ mouseCoords) s = handleCoords mouseCoords s
handleState _  s = s

-- | Function to change universe according
--   to its rules by the time passed
--   detect if the game is over
updateState :: Float -> State -> State
updateState dt (State u us)
  | isWon u && uStage u == 0   = State wonU us
  | isLost u                   = State lostU us
  | not (isWon u)              = State newU us
  | otherwise                  = State u us
  where
    newEnemies = updateZombies dt (State u us) 
    newDefense = updatePlants dt (State u us)
    newSuns    = updateSuns dt (State u us)
    newCards   = updateCards dt (State u us)
    newTime    = (uTime u) + dt
    wonU       = u
     { uScreen = newScreen (uLevelNum u) 1
     , uStage  = 1
     }
    lostU      = u
     { uScreen = lost }
    newU       = u
        { uEnemies = newEnemies
        , uDefense = newDefense
        , uCards   = newCards
        , uSuns    = newSuns
        , uTime    = newTime
        }


perform :: IO()
perform = play
          screen
          white
          60
          initState
          drawState
          handleState
          updateState
