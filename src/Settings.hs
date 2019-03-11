{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Type
import Structure.Object
import Graphics.Gloss
import AI 
import UI

begginnerCoords :: Float
begginnerCoords = -200

boxSize :: Float
boxSize = 30

-- | Predefined wave of enemies
sampleZombies :: [Zombie]
sampleZombies = 
  [ Zombie Basic (150,  100) 0 
  , Zombie Buckethead (150,    0) 0 
  , Zombie Basic (150, -100) 0
  ]

-- | Predefined defense structure
samplePlants :: [Plant]
samplePlants =
  [ Plant PeasShooter (-200,  100) 0 []
  , Plant PeasShooter (-200,    0) 0 []
  , Plant PeasShooter (-200, -100) 0 []
  ]
 
-- | Predefined sunflowers structure
sampleSunflowers :: [Sunflower]
sampleSunflowers = 
    [ Sunflower (-160, 100) 0 [] 
    ]

cards :: [Card]
cards = initCards [PeasShooter] 
  (cardsMarginX - fromIntegral screenWidth / 2 + cardWidth / 2,
  fromIntegral screenHeight / 2 - cardsMarginY - cardHeight / 2)

-- | Starter universe
initUniverse :: Universe
initUniverse = Universe 
               sampleZombies
               samplePlants
               sampleSunflowers
               False
               0
               cards
               blank
