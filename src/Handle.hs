{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handle where

import           Type
import           Settings
import           Data.Maybe
import           Data.List
import           Utils
import           System.Directory

-- | Function to change the universe, based on the
--   left click of the mouse inside the screen border
handleCoords :: Coords -> State-> State
handleCoords mc s
  | isWon u && uStage u == NewCard   = s { sUniverse = handleNext u }
  | isLost u && uStage u == Game     = s { sUniverse = getLevel $ uLevelNum u }
  | isWon u && uStage u == NextLevel = newU $ sLevels s
  | uStage u == Menu                 = s { sUniverse = startGame mc u } 
  | not (isWon u)                    = s { sUniverse = goToMenu mc u }
  | otherwise                        = s
  where
    newU (n:us) = s { sUniverse = n
                    , sLevels   = us
                    }
    newU []     = s
    u           = sUniverse s              
 
-- | Function to handle if the player clicked on the menu
--   button or proceed with the game.
--   Checks if the mouse coord lie in the range of the menu
--   button, if yes -> go to menu screen
--   if no -> update universe, based on the place, where mouse
--   coords clicked
goToMenu :: Coords -> Universe -> Universe
goToMenu mc u = changeScreen
  where
    isButton                  = checkMouse mc (600, 260) 175 70
    changeScreen              = if isButton
                                  then newU
                                  else handleU
    newU                      = u
                    { uStage  = Menu }
    handleU                   = u
                   { uDefense = newDefense
                   , uCards   = newCards
                   , uSuns    = newSuns
                   , uMoney   = updMoney
                   }
    (newDefense, updCs, newM) = handlePlants mc u
    newCards                  = handleCards mc u updCs
    (newSuns, updMoney)       = if newM == uMoney u
                                  then handleSuns mc u newM
                                  else (uSuns u, newM)

-- | Function to show middle screen after winning the
--   level
handleNext :: Universe -> Universe
handleNext u = u
         { uStage  = NextLevel }

-- | Function to handle picking plant card
-- * if any card is active and mouse was clicked
--   deactivate the active card
-- * if player clicked on the player invert
--   its property of active
handleCards :: Coords -> Universe -> [Card] -> [Card]
handleCards mXY u cs
  | any isActive cs = map deactivate cs
  | otherwise       = map activate cs
  where
    deactivate c = if isActive c
                     then invertCardActive c
                     else c
    activate c = if cond
                    then invertCardActive c
                    else c
      where
        cond = mouseClickedCard && m >= cMoney (plantType c) && cTime c == 0
        mouseClickedCard = checkMouse mXY (cCoords c) cardWidth cardHeight
        m = uMoney u

-- | Function to handle plants on mouse click
--
-- * perform adding plants to the game border
--
-- * perform collecting suns
handlePlants :: Coords -> Universe -> ([Plant], [Card], Int)
handlePlants mc u = handle (ps, money)
  where
    handle = addPlant mc u
           . collectSun mc u
    ps     = uDefense u
    money  = uMoney u

-- | Function to plant the card.
--
--   Find possible active card of plant or nothing
--   Find possible cell coordinates or nothing 
addPlant
  :: Coords
  -> Universe
  -> ([Plant], Int)
  -> ([Plant], [Card], Int)
addPlant mc u (ps, money) = (newPs, newC, m) 
  where
    (newPs, c, m) = putPlant (active, coords) ps money
    active        = listToMaybe (filter isActive cs)
    coords        = getCoords mc cellCoords
    cs            = uCards u
    newC          =
      case c of
        Nothing    -> cs
        Just card  -> card : filter (\x -> not (isActive x)) cs

-- | Function to plant the card.
-- * if none card is active -> return same plants
-- * if coords are wrong    -> return same plants
-- * else                   -> create new plant on coords
--                             and with type stated by card
putPlant
  :: (Maybe Card, Maybe Coords)
  -> [Plant]
  -> Int
  -> ([Plant], Maybe Card, Int)
putPlant (active, coords) ps m = 
  case (active, coords) of
    (Just card, Just xy) -> if any (collisions xy) pXY || canBuy card
                               then (ps, Nothing, m)
                               else (newP card xy, newC card, sub card)
    _                    -> (ps, Nothing, m)
  where
    collisions xy = checkCollision size size size size xy
    canBuy card   = cMoney (plantType card) > m
    size          = plantSize - 10
    sub card      = m - cMoney (plantType card)
    pXY           = map pCoords ps
    newC c        = Just (c { cTime = cFrequency (plantType c) })
    newP c xy     = Plant pt xy 0 [] (pStarterTimer pt) : ps
      where
        pt = plantType c


-- | Function to see if the mouse clicked inside
--   cell of the game board, where plant may be
--   planted
getCoords :: Coords -> [Coords] -> Maybe Coords
getCoords _xy []      = Nothing
getCoords xy (x : xs) = if checkMouse xy x cellWidth cellHeight
                        then Just x
                        else getCoords xy xs

-- | Function to collect suns, which were fallen from the
--   sunflowers. First, checks if any card is active, 
--   cause cannot plant/collect sun at the same time.
--   Takes only sunflowers from plants and checks, if mouse
--   coordinate intersects with one of the sun in sunflower
--   If true, deletes the sun and adds money, else returns 
--   the same list of plant and same amount of money
collectSun :: Coords -> Universe -> ([Plant], Int) -> ([Plant], Int)
collectSun mc u (ps, m) = if active
                             then (ps, m)
                             else (pss ++ remove, m + addMoney)
  where
    active     = any isActive cs
    remove     = map (\p -> p { pBullet = removeSun mc (pBullet p) }) sfs
    collection = map (isCollected mc) (map pBullet sfs)
    (sfs, pss) = filterPlant ps
    cs         = uCards u
    addMoney
      | True `elem` collection = 25
      | otherwise              = 0

-- | Function to remove sun
--   Finds index of the first sun, which was clicked (as
--   there maybe several suns on the same coordinates);
--   splits list by the index, where first list will
--   contain first clicked sun. Then filters the first list
--   by removing this sun and concatenates with the rest of
--   the suns.
--   If there's no sun, return the same list
removeSun :: Coords -> [Projectile] -> [Projectile]
removeSun mc ss = newBullet
  where
    toDelete  = findIndex (\x -> checkMouse mc (prCoords x) 88.5 88.5) ss
    newBullet = 
      case toDelete of
        Just index -> some index
        Nothing    -> ss
      where
        some i      = filter predicate (fst (splitSuns i)) 
                    ++ snd (splitSuns i)
        splitSuns i = splitAt (i + 1) ss
        predicate s = not (checkMouse mc (prCoords s) 88.5 88.5)

-- | Function to divide list of plants by the plant types.
--   Creates two list: sunflowers, other type of plants
filterPlant :: [Plant] -> ([Plant], [Plant])
filterPlant ps = (sfs, pss)
  where
    sfs = filter (\p -> pType p == Sunflower) ps
    pss = filter (\p -> pType p /= Sunflower) ps    

-- | Function to see, if the mouse clicked on suns coordinate
--   if at least one coordinate satisfy the predicate,
--   return True
isCollected :: Coords -> [Projectile] -> Bool
isCollected mc ss = collected
  where
    collected = any (\x -> checkMouse mc (prCoords x) 88.5 88.5) ss

-- | Function to handle universe suns,
--   First, checks if any card is active, 
--   cause cannot plant/collect sun at the same time.
--   if mouse coordinate intersects with one of the sun
--   If true, deletes the sun and adds money, else returns 
--   the same list of projectiles and same amount of money
handleSuns :: Coords -> Universe -> Int -> (([Projectile], Float), Int)
handleSuns mc u m = if active
                      then ((ss, t), m)
                      else ((remove, t), m + addMoney)
  where
    active     = any isActive cs
    remove     = removeSun mc ss
    collection = isCollected mc ss
    cs         = uCards u
    (ss, t)    = uSuns u
    addMoney
      | collection = 25
      | otherwise  = 0


checkMouse :: Coords -> Coords -> Float -> Float -> Bool
checkMouse (mX, mY) (cX, cY) width height = mX >= leftX && mX <= rightX
                                         && mY >= leftY && mY <= rightY
  where
    leftX    = cX - width / 2
    rightX   = cX + width / 2
    leftY    = cY - height / 2
    rightY   = cY + height / 2

invertCardActive :: Card -> Card 
invertCardActive card = card { isActive = not active }
  where
    active = isActive card

-- | Function to add char to the username.
--   Restricted to ten letters in the username
--   Because of the way username is printed,
--   new char is added to the end of the list
addChar :: Char -> State -> State
addChar c s = if length (sName s) > 10
                    then s
                    else s { sName = ((sName s) ++ [c]) }

-- | Function to remove letter from the username
--   In order, to perform it purely, reverses string
--   and takes the head and returns the reversed end
--   of list. If username is empty, does nothing
deleteChar :: State -> State
deleteChar s = change name
  where
    name = reverse (sName s)
    change (_c:left) = s { sName = reverse left }
    change []        = s

-- | Function to perform changings of the username
-- * if clicked rename -> allow adding chars to username
--                        and removing chars from username
-- * if clicked delete -> deletes the saving and creates
--                        new game from the beginning,
--                        should enter new user name to proceed
-- * if clicked ok     -> stores all the editting
-- * if clicked cancel -> cancels all the editting
-- * otherwise         -> does nothing
checkField :: Coords -> State -> IO State
checkField mc s
  | isRename  = return $ s { sEdit = True }
  | isDelete  = return $ startNew
  | isOK      = saveName s
  | isCancel  = cancelEdit s
  | otherwise = return $ s
  where
    isRename = checkMouse mc (-136, -160) 248 40
    isDelete = checkMouse mc (135, -160) 248 40
    isOK     = checkMouse mc (-136, -207) 248 40 && (length (sName s) /= 0)
    isCancel = checkMouse mc (135, -227) 248 40
    startNew = initState { sUniverse = 
                            (sUniverse initState) { uStage = EditName } }   
        
-- | Function to handle state, when player canceled
--   all its edittings.
--   Read userName file to know the old username of
--   saving.
-- * If there were no
--   username at all        -> does not allow to go to the
--   (game just started)       main menu screen
-- * If the username exists -> load saving file and
--                             return to the main
--                             menu
cancelEdit :: State -> IO State
cancelEdit s = do name <- readFile "save/userName.txt"
                  case length name of
                    0 -> return s
                    _ -> loadSaving name
  where
    loadSaving n = do let path = "save/" ++ n ++ ".txt"
                      strState <- readFile path
                      let st    = read $ strState
                      return st { sUniverse = (sUniverse st) { uStage = Menu } }

-- | Function to handle state, when the player
--   wants to apply its changes.
--   Reads old username.
-- * If it first session    -> writes the file new
--                             username, saves state
--                             in the file, returns
--                             to main Menu
-- * If name did not change -> saves state in the
--                             file
-- * If name changed        -> rename saving file
--                             rewrite username
--                             save state
saveName :: State -> IO State
saveName s = do name <- readFile "save/userName.txt"
                case () of _
                            | length name == 0 -> createUser
                            | name == sName s  -> saveState goBack
                            | otherwise        -> changeName name
  where
    createUser   = do writeFile "save/userName.txt" (sName s)
                      saveState goBack
    changeName n = do writeFile "save/userName.txt" (sName s)
                      let path = "save/" ++ sName s ++ ".txt" 
                      let path2 = "save/" ++ n ++ ".txt"
                      renameFile path2 path
                      saveState $ s { sUniverse = u { uStage = Menu}}
    goBack       = s { sEdit     = False
                     , sUniverse = u { uStage = Menu }
                     }
    u            = sUniverse s 

-- | Function to handle user clicks in the main Menu
-- * if clicked start -> change state to player state
-- * if clicked edit  -> change state to change username
--                       state
startGame :: Coords -> Universe -> Universe
startGame mc u
  | isStart   = u { uStage = Menu }
  | isEdit    = u { uStage = EditName }
  | otherwise = u
  where
    isStart = checkMouse mc (-275, 55) 250 110
    isEdit  = checkMouse mc (-574, 170) 212 40

-- | Function to save game in file
--   Writes to the file with the name, which
--   corresponds to player's username
saveState :: State -> IO State
saveState s = do
  let path = "save/" ++ sName s ++ ".txt"
  writeFile path $ show s
  return s
