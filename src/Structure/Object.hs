{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Structure.Object where

import Graphics.Gloss

zombie :: Picture
zombie = color (dark aquamarine) (circleSolid 10)

plant :: Picture
plant = color green (rectangleSolid 20 20)

projectile :: Picture
projectile = circleSolid 5

field :: Picture
field = line [(-200,  -50), (200,  -50)]
     <> line [(-200, -150), (200, -150)]
     <> line [(-200,   50), (200,   50)]
     <> line [(-200,  150), (200,  150)]

lost :: Picture
lost = Translate (-200) 0 (scale 0.5 0.5 (text "You lost"))

win :: Picture
win = Translate (-200) 0 (scale 0.5 0.5 (text "You won"))

screen :: Display
screen = InWindow "Scene" (400, 400) (10, 10)

structures :: Picture
structures = Translate   40  0 zombie 
          <> Translate (-40) 0 plant


