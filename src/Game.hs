{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}

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
import System.Exit
import System.Directory

-- | Function to render universe
-- * if in menu screen    -> draws menu with the username
-- * if player edits name -> draws menu with the window
--                           to edit name
-- * if screen is between -> draw corresponding the the
--   new card and next       level and stage of the game
--   level                   picture
-- * if player won        -> draw new card
-- * if player lost       -> draw lose screen
-- * if in the game       -> draw field
drawState :: State -> Picture
drawState s
  | uStage u == Menu      = newScreen (uLevelNum u) Menu 
                         <> Translate (-595) 216.5
                            (scale 0.7 0.7 (strPicture (sName s)))
  | uStage u == EditName  = newScreen (uLevelNum u) EditName
                         <> Translate 0 85 (strPicture (sName s))
                         <> Translate (-595) 216.5
                            (scale 0.7 0.7 (strPicture (sName s))) 
  | uStage u == NextLevel = pic <> newScreen (uLevelNum u) NextLevel
  | uStage u == NewCard   = pic <> newScreen (uLevelNum u) NewCard
  | isLost u 
    && uStage u == Game   = pic <> lost
  | otherwise             = pic 
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
-- * if player clicked with mouse -> handle depending on the
--                                   coords
-- * if player used keyboard      -> changes name, if in edit state
-- * if player used backdpace but -> changed name, if in edit state
-- * other interaction            -> leaves unchanged
handleState :: Event -> State -> State
handleState (EventKey (MouseButton LeftButton)
               Down _ mouseCoords) s       = handleCoords mouseCoords s
handleState (EventKey (Char c) Down _ _) s = if sEdit s 
                                              then addChar c s
                                              else s
handleState (EventKey (SpecialKey KeyDelete)
            Down _ _) s                    = if sEdit s
                                              then deleteChar s
                                              else s
handleState _  s = s

-- | Function to change universe according
--   to its rules by the time passed
--   detect if the game is over
-- * if player won    -> change stage of the game to
--                       display new card opened to player
-- * if lost          -> leave state unchanged
-- * if still playing -> update universe fields based on
--                       rules defined in universe
-- * else             -> leaves unchanges
updateState :: Float -> State -> State
updateState dt s
  | isWon u 
    && uStage u == Game = s { sUniverse = wonU }
  | isLost u            = s
  | not (isWon u)
    && uStage u == Game = s { sUniverse = newU }
  | otherwise           = s
  where
    newEnemies = updateZombies dt s 
    newDefense = updatePlants dt s
    newSuns    = updateSuns dt s
    newCards   = updateCards dt s
    newTime    = (uTime u) + dt
    u          = sUniverse s
    wonU       = u
     { uStage  = NewCard } 
    newU       = u
        { uEnemies = newEnemies
        , uDefense = newDefense
        , uCards   = newCards
        , uSuns    = newSuns
        , uTime    = newTime
        }

-- | Function used, because savings of the game
--   are performed. As nothing unpure happens
--   within drawStateIO, just calls drawState
drawStateIO :: State -> IO Picture
drawStateIO = return . drawState

-- | Function to perfrom savings. As savings is
--   not unpure, uses IO.
-- * if pressed F2    -> save game
-- * if mouse clicked and in the stage
--    of editing name -> perform needed editing
-- * if pressed Esc -> save game and exit
-- * else -> call handleState
handleStateIO :: Event -> State -> IO State
handleStateIO (EventKey (SpecialKey KeyF2)
               Down _ _) s  = saveState s
handleStateIO (EventKey (MouseButton LeftButton)
               Down _ mc) s = if uStage (sUniverse s) == EditName
                                then checkField mc s
                                else return $ handleCoords mc s
handleStateIO (EventKey (SpecialKey KeyEsc)
               Down _ _) s  = do _ <- saveState s
                                 exitSuccess
handleStateIO e s            = return $ handleState e s


updateStateIO :: Float -> State -> IO State
updateStateIO dt = return . updateState dt

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
            let fileName = "save/userName.txt"
            isExist <- doesFileExist fileName
            if not isExist
              then writeFile fileName ""       
              else return ()
            name <- readFile fileName
            case (length name) of
              0 -> gameIO $ initState 
              _ -> do
                      let path = "save/" ++ name ++ ".txt"
                      strState <- readFile path
                      let s    = read $ strState
                      gameIO $ s { sUniverse = (sUniverse s) { uStage = Menu } }
