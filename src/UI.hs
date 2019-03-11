module UI where

import Graphics.Gloss.Data.Color(Color, black, green, yellow)
import Graphics.Gloss.Data.Picture (Picture, color, rectangleSolid, blank, translate, pictures)

import Type (Card (Card), isActive, cardColor, plantType,
 PlantType (PlantOne, PlantTwo), Coords, cardCoords)

cardWidth :: Float
cardWidth = 60

cardHeight :: Float
cardHeight = 75

cardLiningThickness :: Float
cardLiningThickness = 3

cardLiningColor :: Color
cardLiningColor = black

cardsDistance :: Float
cardsDistance = 20

cardsMarginX :: Float
cardsMarginX = 50

cardsMarginY :: Float
cardsMarginY = 10

drawCard :: Card -> Picture
drawCard card = translate x y
                (pictures [if (isActive card) then drawLining else blank, 
                color (cardColor card) (rectangleSolid cardWidth cardHeight)])
  where
    drawLining = color cardLiningColor 
      (rectangleSolid (cardWidth + cardLiningThickness * 2) (cardHeight + cardLiningThickness * 2))
    (x, y) = cardCoords card

drawCards :: [Card] -> Picture
drawCards [] = blank
drawCards (c:cs) = drawCard c <> drawCards cs

mapPlatTypeToCardColor :: PlantType -> Color
mapPlatTypeToCardColor PlantOne = green
mapPlatTypeToCardColor PlantTwo = yellow

initCards :: 
  [PlantType]
  -> Coords -- ^ Coordinatates of the first card
  -> [Card]
initCards [] _ = []
initCards (p:ps) (x, y) = [Card (mapPlatTypeToCardColor p) False p (x, y)] ++ initCards ps (x + cardWidth + cardsDistance, y)

checkMouseClick :: Coords -> Card -> Bool
checkMouseClick (mX, mY) card = mX >= cX - cardWidth / 2 && mX <= cX + cardWidth / 2 &&
                                mY >= cY - cardHeight / 2 && mY <= cY + cardHeight / 2
  where
    (cX, cY) = cardCoords card

invertCardActive :: Card -> Card 
invertCardActive card = Card (cardColor card) (not (isActive card)) (plantType card) (cardCoords card)
