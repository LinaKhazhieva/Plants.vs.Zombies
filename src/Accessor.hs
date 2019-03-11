{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Accessor where

import Type
import Structure.Object
import Graphics.Gloss

-- | Accessor for the speed of the zombies
zSpeed :: ZombieType -> Float
zSpeed ZombieOne = 4
zSpeed ZombieTwo = 5
zSpeed ZombieThree = 7

-- | Accessor of the zombies health 
zHealth :: ZombieType -> Int
zHealth _z = 10

-- | Accessor of the zombies strength
zStrength :: ZombieType -> Int
zStrength _z = 10

-- | Accessor to render zombie type
zPicture :: ZombieType -> Picture
zPicture _z = zombie

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

-- | Accessor to render sunflower 
--sPicture :: Sunflower -> Picture 
--sPicture _s = sunFlower

