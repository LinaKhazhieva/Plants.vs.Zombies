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
                }
  where
    newCards   = handleCards mouseCoords (uCards u)
    newDefense = handlePlants mouseCoords u

-- | Function to handle picking plant card
-- * if any card is active and mouse was clicked
--   deactivate the active card
-- * if player clicked on the player invert
--   its property of active
handleCards :: Coords -> [Card] -> [Card]
handleCards mXY _cards
  | any isActive _cards = cards
  | otherwise           = map (\c -> 
                           if checkMouse mXY (cCoords c) cardWidth cardHeight
                           then invertCardActive c
                           else c) _cards

handlePlants :: Coords -> Universe -> [Plant]
handlePlants mc u = handle ps
  where
    handle = addPlant mc u
           . collectSun mc
    ps     = uDefense u

-- | Function to plant the card. First checks if
--   some card is chosen, then checks if the coords
--   are corresponding to the right coordinates
--   of the cell
addPlant :: Coords -> Universe -> [Plant] -> [Plant]
addPlant mc u = putPlant (active, coords)
  where
    active = listToMaybe (filter isActive cs)
    coords = getCoords mc cellCoords
    cs = uCards u

-- | Function to plant the card.
-- * if none card is active -> return same plants
-- * if coords are wrong    -> return same plants
-- * else                   -> create new plant on coords
--                             and with type stated by card
putPlant :: (Maybe Card, Maybe Coords) -> [Plant] -> [Plant]
putPlant (active, coords) ps = 
  case (active, coords) of
    (Nothing, _) -> ps
    (_, Nothing) -> ps
    (Just card, Just xy) -> if True `elem` (collisions xy)
                               then ps
                               else newP card xy : ps
  where
    collisions xy = map (checkCollision size size size size xy) pXY 
    size          = plantSize
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

collectSun :: Coords -> [Plant] -> [Plant]
collectSun mc ps = pss ++ map (removeSun mc) sfs
  where
    sfs = filter (\p -> pType p == Sunflower) ps
    pss = filter (\p -> pType p /= Sunflower) ps

removeSun :: Coords -> Plant -> Plant
removeSun mc p = remove ss
  where
    remove []       = p
    remove (x : xs) = if checkMouse mc (prCoords x) plantSize plantSize
                        then p { pBullet = xs }
                        else remove xs
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
