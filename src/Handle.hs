{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handle where

import Type
import Settings
import Data.Maybe
import Utils

-- | Function to change the universe, based on the
--   left click of the mouse inside the screen border
handleCoords :: Coords -> Universe -> Universe
handleCoords mouseCoords u = u 
                { uDefense = newDefense
                , uCards   = newCards
                , uMoney   = newMoney
                }
  where
    newCards               = handleCards mouseCoords u (uCards u)
    (newDefense, newMoney) = handlePlants mouseCoords u

-- | Function to handle picking plant card
-- * if any card is active and mouse was clicked
--   deactivate the active card
-- * if player clicked on the player invert
--   its property of active
handleCards :: Coords -> Universe -> [Card] -> [Card]
handleCards mXY u _cards
  | any isActive _cards = cards
  | otherwise           = map activate  _cards
  where
    activate c = if mouseClickedCard && m >= cMoney (plantType c)
                    then invertCardActive c
                    else c
      where
        mouseClickedCard = checkMouse mXY (cCoords c) cardWidth cardHeight
        m = uMoney u

-- | Function to handle plants on mouse click
--
-- * perform adding plants to the game border
--
-- * perform collecting suns
handlePlants :: Coords -> Universe -> ([Plant], Int)
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
addPlant :: Coords -> Universe -> ([Plant], Int) -> ([Plant], Int)
addPlant mc u (ps, money) = putPlant (active, coords) ps money
  where
    active = listToMaybe (filter isActive cs)
    coords = getCoords mc cellCoords
    cs     = uCards u

-- | Function to plant the card.
-- * if none card is active -> return same plants
-- * if coords are wrong    -> return same plants
-- * else                   -> create new plant on coords
--                             and with type stated by card
putPlant :: (Maybe Card, Maybe Coords) -> [Plant] -> Int -> ([Plant], Int)
putPlant (active, coords) ps m = 
  case (active, coords) of
    (Nothing, _) -> (ps, m)
    (_, Nothing) -> (ps, m)
    (Just card, Just xy) -> if any (collisions xy) pXY || canBuy card
                               then (ps, m)
                               else (newP card xy : ps, m - sub card)
  where
    collisions xy = checkCollision size size size size xy
    canBuy card   = cMoney (plantType card) > m
    size          = plantSize
    sub card      = cMoney (plantType card)
    pXY           = map pCoords ps
    newP c xy     = Plant pt xy 0 [] (pStarterTimer pt)
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

collectSun :: Coords -> Universe -> ([Plant], Int) -> ([Plant], Int)
collectSun mc u (ps, m) = if active
                             then (ps, m)
                             else (pss ++ remove, m + addMoney)
  where
    active     = any isActive cs
    remove     = map (removeSun mc) sfs
    collection = map (isCollected mc) sfs
    (sfs, pss) = filterPlant ps
    cs = uCards u
    addMoney
      | True `elem` collection = 25
      | otherwise              = 0
    
removeSun :: Coords -> Plant -> Plant
removeSun mc p = p { pBullet = newBullet }
  
  where
    newBullet = filter (\x -> not (checkMouse mc 
                (prCoords x) plantSize plantSize)) ss
    ss = pBullet p

filterPlant :: [Plant] -> ([Plant], [Plant])
filterPlant ps = (sfs, pss)
  where
    sfs = filter (\p -> pType p == Sunflower) ps
    pss = filter (\p -> pType p /= Sunflower) ps    

isCollected :: Coords -> Plant -> Bool
isCollected mc p = collected
  where
    collected = any (\x -> checkMouse mc (prCoords x) plantSize plantSize) ss
    ss = pBullet p

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
