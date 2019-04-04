{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Type
import Graphics.Gloss

begginnerCoords :: Float
begginnerCoords = -450

endingCoords :: Float
endingCoords = 300

bottomCoords :: Float
bottomCoords = -280

topCoords :: Float
topCoords = 220

cellWidth :: Float
cellWidth = 50

cellHeight :: Float
cellHeight = 100

boxSize :: Float
boxSize = 30

cardWidth :: Float
cardWidth = 50

cardHeight :: Float
cardHeight = 70

cardLiningThickness :: Float
cardLiningThickness = 3

cardLiningColor :: Color
cardLiningColor = black

cardsDistance :: Float
cardsDistance = 15

cardsMarginX :: Float
cardsMarginX = 50

cardsMarginY :: Float
cardsMarginY = 10

zombieWidth :: Float
zombieWidth = 115

zombieHeight :: Float
zombieHeight = 200

plantSize :: Float
plantSize = 80

peasSize :: Float
peasSize = 20

deltaYProjectile :: Float
deltaYProjectile = 15

deltaXProjectile :: Float
deltaXProjectile = 40

uFrequency :: Float
uFrequency = 6

transparentBlack :: Color
transparentBlack = makeColor 0 0 0 0.57 

cellCoords :: [Coords]
cellCoords =
  [ (-408, 170), (-408, 70), (-408, -30), (-408, -130), (-408, -225)
  , (-325, 170), (-325, 70), (-325, -30), (-325, -130), (-325, -225)
  , (-245, 170), (-245, 70), (-245, -30), (-245, -130), (-245, -225)
  , (-158, 170), (-158, 70), (-158, -30), (-158, -130), (-158, -225)
  , ( -80, 170), ( -80, 70), ( -80, -30), ( -80, -130), ( -80, -225)
  , (   0, 170), (   0, 70), (   0, -30), (   0, -130), (   0, -225)
  , (  80, 170), (  80, 70), (  80, -30), (  80, -130), (  80, -225)
  , ( 155, 170), ( 155, 70), ( 155, -30), ( 155, -130), ( 155, -225)
  , ( 240, 170), ( 240, 70), ( 240, -30), ( 240, -130), ( 240, -225) ]

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
  [-- Plant PeasShooter (-408,  70) 0 [] 0
  -- , Plant PeasShooter (-408, -30) 0 [] 0
 -- , Plant PeasShooter (-408, -130) 0 [] 0
--  , Plant Sunflower   (-330, 100)  0 [] 4
  ]
 
cards :: [Card]
cards = initCards [PeasShooter, Sunflower] (-570,  256.5)

initCards :: 
  [PlantType]
  -> Coords -- ^ Coordinatates of the first card
  -> [Card]
initCards [] _ = []
initCards (p:ps) (x, y) = [Card False p (x, y) (cFrequency p)]
                       ++ initCards ps (x + cardWidth + cardsDistance, y)

-- | Starter universe
initUniverse :: Universe
initUniverse = Universe 
               sampleZombies
               samplePlants
               cards
               ([], uFrequency)
               blank
               False
               0
               50
