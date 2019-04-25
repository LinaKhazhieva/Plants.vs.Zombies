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
wallnut :: Picture
wallnut = pic 
  where 
    pic = loadPicture "images/Wallnut.bmp"


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


wallnutCard :: Picture
wallnutCard= scale 0.52 0.52 pic
  where
    pic = loadPicture "images/wallnutcard.bmp"

one :: Picture
one = pic
  where
    pic = loadPicture "images/one.bmp"

two :: Picture
two = pic
  where
    pic = loadPicture "images/two.bmp"

three :: Picture
three = pic
  where
    pic = loadPicture "images/three.bmp"

four :: Picture
four = pic
  where
    pic = loadPicture "images/four.bmp"

five :: Picture
five = pic
  where
    pic = loadPicture "images/five.bmp"

six :: Picture
six = pic
  where
    pic = loadPicture "images/six.bmp"

seven :: Picture
seven = pic

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

sunflowerAlmanac :: Picture
sunflowerAlmanac = pic
  where
    pic = loadPicture "images/sunflowerAlmanac.bmp"

wallnutAlmanac :: Picture 
wallnutAlmanac = pic 
  where 
    pic = loadPicture "images/wallnutAlmanac.bmp"    

win :: Picture
win = Translate (-200) 0 (scale 0.5 0.5 (text "You won"))

zombieNote :: Picture 
zombieNote = scale 0.5 0.5  pic 
  where
    pic = loadPicture "images/zombieNote.bmp"

finalNote :: Picture 
finalNote = scale 0.5 0.5  pic 
  where
    pic = loadPicture "images/finalNote.bmp"    

zombieNoteNextLvl :: Picture
zombieNoteNextLvl = scale 0.5 0.5 pic 
  where 
    pic = loadPicture "images/zombieNoteNextLvl.bmp"


screen :: Display
screen = InWindow "Scene" (screenWidth, screenHeight) (10, 10)

