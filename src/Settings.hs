{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Type
import Structure.Object
import Graphics.Gloss

begginnerCoords :: Float
begginnerCoords = -200

endingCoords :: Float
endingCoords = 150

boxSize :: Float
boxSize = 30

cardWidth :: Float
cardWidth = 50

cardHeight :: Float
cardHeight = 45

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

-- | Predefined wave of enemies
sampleZombies :: [Zombie]
sampleZombies = 
  [ Zombie Basic      (450,  100) 0 0 
  , Zombie Buckethead (450,    0) 0 0
  , Zombie Basic      (450, -100) 0 0
  ]

-- | Predefined defense structure
samplePlants :: [Plant]
samplePlants =
  [ Plant PeasShooter (-200,  100) 0 [] 0
  , Plant PeasShooter (-200,    0) 0 [] 0
  , Plant PeasShooter (-200, -100) 0 [] 0
  ]
 
-- | Predefined sunflowers structure
sampleSunflowers :: [Sunflower]
sampleSunflowers = 
    [ Sunflower (-160, 100) 0 [] 4
    ]

cards :: [Card]
cards = initCards [PeasShooter] 
  (cardsMarginX - fromIntegral screenWidth / 2 + cardWidth / 2,
  fromIntegral screenHeight / 2 - cardsMarginY - cardHeight / 2)

initCards :: 
  [PlantType]
  -> Coords -- ^ Coordinatates of the first card
  -> [Card]
initCards [] _ = []
initCards (p:ps) (x, y) = [Card (pCardColor p) False p (x, y)] ++ initCards ps (x + cardWidth + cardsDistance, y)

-- | Starter universe
initUniverse :: Universe
initUniverse = Universe 
               sampleZombies
               samplePlants
               sampleSunflowers
               cards
               blank
               False
               0
