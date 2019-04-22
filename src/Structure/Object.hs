{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
{-# LANGUAGE OverloadedStrings #-}

module Structure.Object where

import Graphics.Gloss
import System.IO.Unsafe (unsafePerformIO)
import Structure.Alphabet

screenWidth :: Int
screenWidth = 1400

screenHeight :: Int
screenHeight = 600

bucketheadZombie :: Picture
bucketheadZombie = scale 0.5 0.5 pic
  where
    pic = loadPicture "images/buckethead.bmp"

basicZombie :: Picture
basicZombie = scale 0.5 0.5 pic
  where
    pic = loadPicture "images/zombiebasic.bmp"

plant :: Picture
plant = pic
  where
    pic = loadPicture "images/peasshooter.bmp"

projectile :: Picture
projectile = pic
  where
    pic = loadPicture "images/ProjectilePea.bmp"

sun :: Picture 
sun = scale 0.5 0.5 pic
  where
    pic = loadPicture "images/sun.bmp"

sunflower :: Picture 
sunflower = pic
  where
    pic = loadPicture "images/sunflower.bmp"

cardBlock :: Picture
cardBlock = scale 0.5 0.5 pic
  where
    pic = loadPicture "images/cardblock.bmp"

peasshooterCard :: Picture
peasshooterCard = scale 0.52 0.52 pic
  where
    pic = loadPicture "images/peasshootercard.bmp"

sunflowerCard :: Picture
sunflowerCard = scale 0.52 0.52 pic
  where
    pic = loadPicture "images/sunflowercard.bmp"

menu :: Picture
menu = pic
  where
    pic = loadPicture "images/menu.bmp"

user :: Picture
user = pic
  where
    pic = loadPicture "images/user.bmp"

menuButton :: Picture
menuButton = pic
  where
    pic = loadPicture "images/menu_button.bmp"

checkF :: Picture
checkF = pic
  where
    pic = loadPicture "images/check.bmp"

field :: Picture
field = pic 
     <> Translate (-461.25) 256.5 cardBlock
  where
    pic = unsafePerformIO $ loadBMP "images/Background.bmp"

lost :: Picture
lost = pic
  where 
    pic = loadPicture "images/lose.bmp" 

levelOne :: Picture
levelOne = pic
  where
    pic = loadPicture "images/level1.bmp"

win :: Picture
win = Translate (-200) 0 (scale 0.5 0.5 (text "You won"))

screen :: Display
screen = InWindow "Scene" (screenWidth, screenHeight) (10, 10)
