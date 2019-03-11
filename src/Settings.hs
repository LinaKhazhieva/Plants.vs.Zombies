{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Type
import Graphics.Gloss

-- | Predefined wave of enemies
sampleZombies :: [Zombie]
sampleZombies = 
  [ Zombie ZombieOne (150,  100) 0 
  , Zombie ZombieTwo (150,    0) 0 
  , Zombie ZombieThree (150, -100) 0
  ]

-- | Predefined defense structure
samplePlants :: [Plant]
samplePlants =
  [ Plant PlantOne (-200,  100) 0 []
  , Plant PlantOne (-200,    0) 0 []
  , Plant PlantOne (-200, -100) 0 []
  ]
 
-- | Predefined sunflowers structure
sampleSunflowers :: [Sunflower]
sampleSunflowers = 
    [ Sunflower (-150, 100) 0 [] 
    ]

-- | Starter universe
initUniverse :: Universe
initUniverse = Universe 
               sampleZombies
               samplePlants
               sampleSunflowers
               False
               0
               blank
