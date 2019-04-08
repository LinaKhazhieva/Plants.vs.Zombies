{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handle where

import Type
import Settings
import Data.Maybe
import Data.List
import Utils

-- | Function to change the universe, based on the
--   left click of the mouse inside the screen border
handleCoords :: Coords -> State-> State
handleCoords mouseCoords (State u [])
  | isWon u && uStage u == 1   = State (handleNextScreen u) []
  | not (isWon u)              = State (handleProcess mouseCoords u) []
  | otherwise                  = State u []
handleCoords mouseCoords (State u (next:us))
  | isWon u && uStage u == 1   = State (handleNextScreen u) (next:us)
  | isWon u && uStage u == 2   = State next us
  | not (isWon u)              = State (handleProcess mouseCoords u) (next:us)
  | otherwise                  = State u (next:us)              
 

handleProcess :: Coords -> Universe -> Universe
handleProcess mc u = u
        { uDefense = newDefense
        , uCards   = newCards
        , uSuns    = newSuns
        , uMoney   = updMoney
        }
  where
    (newDefense, updCs, newM) = handlePlants mc u
    newCards                  = handleCards mc u updCs
    (newSuns, updMoney)       = if newM == uMoney u
                                  then handleSuns mc u newM
                                  else (uSuns u, newM)

handleNextScreen :: Universe -> Universe
handleNextScreen u = u
         { uScreen = newScreen (uLevelNum u) 2
         , uStage  = 2
         }

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
    size          = plantSize
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
        some i        = filter predicate (fst (splitSuns i)) 
                     ++ snd (splitSuns i)
        splitSuns i   = splitAt (i + 1) ss
        predicate sun = not (checkMouse mc (prCoords sun) 88.5 88.5)

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
