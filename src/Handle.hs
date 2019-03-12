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
checkMouseClick (mX, mY) card = mX >= cX - cardWidth / 2 && mX <= cX + cardWidth / 2 &&
                                mY >= cY - cardHeight / 2 && mY <= cY + cardHeight / 2
  where
    (cX, cY) = cardCoords card

invertCardActive :: Card -> Card 
invertCardActive card = Card (cardColor card) (not (isActive card)) (plantType card) (cardCoords card)
