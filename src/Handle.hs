{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handle where

import Type
import Settings

updateCards :: Coords -> [Card] -> [Card]
updateCards mouseCoords _cards
  | any isActive _cards = cards
  | otherwise = map (\c -> if checkMouseClick mouseCoords c
                           then invertCardActive c
                           else c) _cards

checkMouseClick :: Coords -> Card -> Bool
checkMouseClick (mX, mY) card = mX >= leftX && mX <= rightX &&
                                mY >= leftY && mY <= rightY
  where
    leftX    = cX - cardWidth / 2
    rightX   = cX + cardWidth / 2
    leftY    = cY - cardHeight / 2
    rightY   = cY + cardHeight / 2
    (cX, cY) = cardCoords card

invertCardActive :: Card -> Card 
invertCardActive card = card { isActive = not active }
  where
    active = isActive card
