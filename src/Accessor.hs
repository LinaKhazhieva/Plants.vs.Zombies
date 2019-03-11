{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Accessor where

import Type
import Structure.Object
import Graphics.Gloss

-- | Accessor for the speed of the zombies
zSpeed :: ZombieType -> Float
zSpeed Basic = 6
zSpeed Buckethead = 4

-- | Accessor of the zombies health 
zHealth :: ZombieType -> Int
zHealth Basic = 4
zHealth Buckethead = 6

-- | Accessor of the zombies strength
zStrength :: ZombieType -> Int
zStrength _z = 1

-- | Accessor to render zombie type
zPicture :: ZombieType -> Picture
zPicture Basic = basicZombie
zPicture Buckethead = bucketheadZombie

-- | Accessor for the plant
pHealth :: PlantType -> Int
pHealth _p = 10

-- | Accessor for the plant strength
pStrength :: PlantType -> Int 
pStrength _p = 1

-- | Accessor to render plant type
pPicture :: PlantType -> Picture
pPicture _p = plant

-- | Accessor for the sunflower
sHealth :: Sunflower -> Int 
sHealth _s = 10 
