{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Type
import Graphics.Gloss
import Structure.Object()

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
uFrequency = 10

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
--   has delta of 80, to render beautifully
sampleZombies :: [Zombie]
sampleZombies = 
  [ Zombie Basic (608, 50) 0 0  
  , Zombie Basic (740, 50) 0 0
  , Zombie Basic (850, 50) 0 0
  , Zombie Basic (975, 50) 0 0
  , Zombie Basic (1000, 50) 0 0
  ]
-- | Wave concisting of 1 enemy which is used as a filler 
--   insert it in any level to check other attributes 

checkWave :: [Zombie]
checkWave = 
  [Zombie Basic (620, 50) 0 0]

-- | Wave of enemies for level 2 
zombieLvl2 :: [Zombie]
zombieLvl2 =
  [Zombie Basic (620, 150) 0 0
  , Zombie Basic (740, 150) 0 0
  , Zombie Basic (820, 150) 0 0
  , Zombie Basic (920, -50) 0 0
  , Zombie Basic (1020, 50) 0 0
  , Zombie Basic (1020, 50) 0 0
  , Zombie Basic (1020, 50) 0 0
  ] 

-- | Wave of enemies for level 3
zombieLvl3 :: [Zombie]
zombieLvl3 = 
  [ Zombie Basic (620, 250) 0 0  
  , Zombie Basic (1000, -145) 0 0
  , Zombie Basic (1200, 150) 0 0
  , Zombie Basic (1400, 50) 0 0
  , Zombie Basic (1600, 250) 0 0
  , Zombie Basic (1800, 50) 0 0
  , Zombie Basic (2000, -50) 0 0
  , Zombie Basic (2000, -145) 0 0
  , Zombie Buckethead (2000, 150) 0 0
  ]

-- | Wave of enemies for level 4
zombieLvl4 :: [Zombie]
zombieLvl4 = 
  [ Zombie Basic (620, 250) 0 0  
  , Zombie Basic (1000, -145) 0 0
  , Zombie Basic (1200, 50) 0 0
  , Zombie Basic (1400, 50) 0 0
  , Zombie Basic (1600, 250) 0 0
  , Zombie Buckethead (1800, 50) 0 0
  , Zombie Buckethead (1820, 50) 0 0
  , Zombie Basic (1840, 50) 0 0
  , Zombie Buckethead (1860, 150) 0 0
  , Zombie Basic (2020, -50) 0 0
  , Zombie Basic (2040, -145) 0 0
  , Zombie Buckethead (2060, 150) 0 0
  ]

-- | Wave of enemies for level 5
zombieLvl5 :: [Zombie]
zombieLvl5 = 
  [ Zombie Basic (620, 250) 0 0  
  , Zombie Basic (1000, -145) 0 0
  , Zombie Basic (1200, 50) 0 0
  , Zombie Basic (1400, 250) 0 0
  , Zombie Basic (1600, 150) 0 0
  , Zombie Basic (1800, 50) 0 0
  , Zombie Basic (2000, -50) 0 0
  , Zombie Basic (2000, -145) 0 0
  , Zombie Buckethead (2000, 150) 0 0
  , Zombie Basic (2000, 50) 0 0
  , Zombie Basic (2000, 150) 0 0
  , Zombie Buckethead (2000, 150) 0 0
  , Zombie Buckethead (2200, 50) 0 0
  , Zombie Basic (2200, 250) 0 0 
  , Zombie Basic (2300, -145) 0 0 
  , Zombie Basic (2400, -50) 0 0 
  , Zombie Buckethead (2500, 150) 0 0
  , Zombie Buckethead (2520, 150) 0 0
  ]


-- | Predefined defense structure
samplePlants :: [Plant]
samplePlants =
  []
 
cards :: [Card]
cards = initCards [PeasShooter] (-570,  256.5)


initCards :: 
  [PlantType]
  -> Coords -- ^ Coordinatates of the first card
  -> [Card]
initCards [] _ = []
initCards (p:ps) (x, y) = [Card False p (x, y) (cFrequency p)]
                       ++ initCards ps (x + cardWidth + cardsDistance, y)

initState :: State
initState = State [] False level1 [level2, level3, level4, level5 ]

-- | Level 1 Universe 
level1 :: Universe
level1 = Universe 
               sampleZombies
               []
               cards
               ([], uFrequency) 
               False
               0
               150
               One
               EditName

-- | Level 2 Universe 
level2 :: Universe
level2 = Universe
                zombieLvl2
                []
                (initCards [PeasShooter, Sunflower] (-570,  256.5))
                ([], uFrequency) 
                False
                0
                50
                Two
                Game

-- | Level 3 Universe 
level3 :: Universe
level3 = Universe
                zombieLvl3
                []
                (initCards [PeasShooter, Sunflower] (-570,  256.5))
                ([], uFrequency)
                False
                0
                50
                Three
                Game

-- | Level 4 Universe 
level4 :: Universe
level4 = Universe
                zombieLvl4
                []
                (initCards [PeasShooter, Sunflower, Wallnut] (-570,  256.5))
                ([], uFrequency)
                False
                0
                50
                Four
                Game

-- | Level 5 Universe 
level5 :: Universe
level5 = Universe
                zombieLvl5
                []
                (initCards [PeasShooter, Sunflower, Wallnut] (-570,  256.5))
                ([], uFrequency)
                False
                0
                50
                Five
                Game


getLevel :: Level -> Universe
getLevel One    = level1 { uStage = Game }
getLevel Two    = level2
getLevel Three  = level3
getLevel Four   = level4
getLevel Five   = level5
