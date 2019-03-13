{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handle where

import Type
import Settings
import Data.Maybe

handleCoords :: Coords -> Universe -> Universe
handleCoords mouseCoords u = u 
                { uDefense = newDefense
                , uCards   = newCards
                }
  where
    newCards   = handleCards mouseCoords (uCards u)
    newDefense = handlePlants mouseCoords u

handleCards :: Coords -> [Card] -> [Card]
handleCards mXY _cards
  | any isActive _cards = cards
  | otherwise = map (\c -> if checkMouse mXY (cCoords c) cardWidth cardHeight
                           then invertCardActive c
                           else c) _cards

handlePlants :: Coords -> Universe -> [Plant]
handlePlants mc u = putPlantTo
  where
    active = listToMaybe (filter isActive cs)
    cs     = uCards u
    putPlantTo =
      case active of
        Nothing -> uDefense u
        Just c  -> addPlant mc (plantType c) u

addPlant :: Coords -> PlantType -> Universe -> [Plant]
addPlant mc pt u = createPlant
  where
    coords = getCoords mc cellCoords
    createPlant =
      case coords of
        Nothing     -> uDefense u
        Just (x, y) -> newP (x, y) : uDefense u
      where
        newP coords = Plant pt coords 0 [] (pStarterTimer pt)

-- | Function to see if the mouse clicked inside
--   cell of the game board, where plant may be
--   planted
getCoords :: Coords -> [Coords] -> Maybe Coords
getCoords xy [] = Nothing
getCoords xy (x : xs) = if checkMouse xy x cellWidth cellHeight
                        then Just x
                        else getCoords xy xs

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
