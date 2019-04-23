module Game where

import Structure.Object
import Structure.Alphabet
import Type
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Render
import Settings
import Utils
import Update
import Handle
import Save
import System.Exit

-- | Function to render universe
drawState :: State -> Picture
drawState s
  | uStage u == 3             = newScreen (uLevelNum u) 3 
                             <> Translate (-595) 216.5
                                (scale 0.7 0.7 (strPicture (sName s)))
  | uStage u == 4             = newScreen (uLevelNum u) 4
                             <> Translate 0 85 (strPicture (sName s))
                             <> Translate (-595) 216.5
                                (scale 0.7 0.7 (strPicture (sName s))) 
  | uStage u == 2             = pic <> newScreen (uLevelNum u) 2
  | isWon u && uStage u == 0  = pic <> newScreen (uLevelNum u) 0
  | uStage u == 1             = pic <> newScreen (uLevelNum u) 1
  | isLost u && uStage u == 0 = pic <> lost
  | otherwise                 = pic 
  where
    prs      = concat (map pBullet ps)
    zs       = uEnemies u
    ps       = uDefense u  
    cs       = uCards u
    m        = uMoney u
    (ss, _)  = uSuns u
    u        = sUniverse s
    pic      = field <> renderMenuButton       <> drawObject drawPlant  ps
            <> drawProjectiles Sun (prs ++ ss) <> drawProjectiles Pea prs
            <> drawObject drawZombie zs        <> drawObject (drawCard m) cs
            <> drawMoney m

-- | Function to change universe according
--   to its rules by the interaction with the player
handleState :: Event -> State -> State
handleState (EventKey (MouseButton LeftButton)
               Down _ mouseCoords) s = handleCoords mouseCoords s
handleState (EventKey (Char c) Down _ _)
            s = if sEdit s == Rename 
                  then handleMenu c s
                  else s
handleState (EventKey (SpecialKey KeyDelete)
            Down _ _) s = if sEdit s == Rename
                            then deleteChar s
                            else s
handleState _  s = s

-- | Function to change universe according
--   to its rules by the time passed
--   detect if the game is over
updateState :: Float -> State -> State
updateState dt s
  | isWon u && uStage u == 0       = s { sUniverse = wonU }
  | isLost u                       = s
  | not (isWon u) && uStage u == 0 = s { sUniverse = newU }
  | otherwise                      = s
  where
    newEnemies = updateZombies dt s 
    newDefense = updatePlants dt s
    newSuns    = updateSuns dt s
    newCards   = updateCards dt s
    newTime    = (uTime u) + dt
    u          = sUniverse s
    wonU       = u
     { uStage  = 1 } 
    newU       = u
        { uEnemies = newEnemies
        , uDefense = newDefense
        , uCards   = newCards
        , uSuns    = newSuns
        , uTime    = newTime
        }

drawStateIO :: State -> IO Picture
drawStateIO s = return (drawState s)

handleStateIO :: Event -> State -> IO State
handleStateIO (EventKey (SpecialKey KeyF2) Down _ _) s = saveState s
handleStateIO (EventKey (MouseButton LeftButton)
               Down _ mouseCoords) s = if uStage (sUniverse s) == 4
                                          then checkField mouseCoords s
                                          else return $ handleCoords mouseCoords s
handleStateIO (EventKey (SpecialKey KeyEsc) Down _ _) s = do _ <- saveState s
                                                             exitSuccess
handleStateIO e s = return $ handleState e s

updateStateIO :: Float -> State -> IO State
updateStateIO dt = return . updateState dt

game :: String -> IO ()
game name = play screen bgColor fps
  initState { sName = name }
  drawState
  handleState
  updateState 
  where
    bgColor = white
    fps = 60

gameIO :: State -> IO ()
gameIO s = playIO screen bgColor fps
  s
  drawStateIO
  handleStateIO
  updateStateIO 
  where
    bgColor = white
    fps = 60

perform :: IO()
perform = do
            name <- readFile "save/userName.txt"
            case (length name) of
              0 -> gameIO $ initState 
              _ -> do
                      let path = "save/" ++ name ++ ".txt"
                      strState <- readFile path
                      let s    = read $ strState
                      gameIO $ s { sUniverse = (sUniverse s) { uStage = 3 } }
