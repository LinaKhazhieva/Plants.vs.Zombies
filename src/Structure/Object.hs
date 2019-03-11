{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Structure.Object where

import Graphics.Gloss

screenWidth :: Int
screenWidth = 400

screenHeight :: Int
screenHeight = 500

zombieSize :: Float
zombieSize = 10

zombie :: Picture
zombie = color (dark aquamarine) (circleSolid zombieSize)

plant :: Picture
plant = color green (rectangleSolid 20 20)

projectile :: Picture
projectile = circleSolid 5

field :: Picture
field = line [(-200,  -50), (200,  -50)]
     <> line [(-200, -150), (200, -150)]
     <> line [(-200,   50), (200,   50)]
     <> line [(-200,  150), (200,  150)]


screen :: Display
screen = InWindow "Scene" (screenWidth, screenHeight) (10, 10)

structures :: Picture
structures = Translate   40  0 zombie 
          <> Translate (-40) 0 plant


