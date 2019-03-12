{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Type
import Structure.Object
import Graphics.Gloss

begginnerCoords :: Float
begginnerCoords = -450

endingCoords :: Float
endingCoords = 300

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

zombieWidth :: Float
zombieWidth = 115

zombieHeight :: Float
zombieHeight = 200

plantWidth :: Float
plantWidth = 80

plantHeight :: Float
plantHeight = 80

peasSize :: Float
peasSize = 20

deltaYProjectile :: Float
deltaYProjectile = 15

deltaXProjectile :: Float
deltaXProjectile = 40

-- | Predefined wave of enemies
sampleZombies :: [Zombie]
sampleZombies = 
  [ Zombie Basic      (300,  150) 0 0 
  , Zombie Buckethead (300,   50) 0 0
  , Zombie Basic      (300,  -50) 0 0
  ]

-- | Predefined defense structure
samplePlants :: [Plant]
samplePlants =
  [ Plant PeasShooter (-400,  100) 0 [] 0
  , Plant PeasShooter (-400,    0) 0 [] 0
  , Plant PeasShooter (-400, -100) 0 [] 0
  ]
 
-- | Predefined sunflowers structure
sampleSunflowers :: [Sunflower]
sampleSunflowers = 
    [ Sunflower (-330, 100) 0 [] 4
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
