{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Structure.Object where

import Graphics.Gloss
import System.IO.Unsafe (unsafePerformIO)

screenWidth :: Int
screenWidth = 600

screenHeight :: Int
screenHeight = 400

zombieSize :: Float
zombieSize = 10

bucketheadZombie :: Picture
bucketheadZombie = scale 0.1 0.1 (unsafePerformIO $ loadBMP "images/Buckethead_Zombie.bmp" )

basicZombie :: Picture
basicZombie = scale 0.025 0.025 (unsafePerformIO $ loadBMP "images/zombiebasic.bmp")


plant :: Picture
plant = scale 0.025 0.025 (unsafePerformIO $ loadBMP "images/Peashooter.bmp" )

projectile :: Picture
projectile = scale 0.5 0.5 (unsafePerformIO $ loadBMP "images/ProjectilePea.bmp" )

sun :: Picture 
sun = scale 0.1 0.1 (unsafePerformIO $ loadBMP "images/sun.bmp" )

sunflower :: Picture 
sunflower = scale 0.03 0.03 (unsafePerformIO $ loadBMP "images/sunflower.bmp")

screen :: Display
screen = InWindow "Scene" (screenWidth, screenHeight) (10, 10)

field :: Picture
field = scale 0.5 0.5  (unsafePerformIO $ loadBMP "images/Background.bmp" )


lost :: Picture
lost = Translate (-200) 0 (scale 0.5 0.5 (text "You lost"))

win :: Picture
win = Translate (-200) 0 (scale 0.5 0.5 (text "You won"))
