{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}

module Structure.Alphabet where

import Graphics.Gloss
import System.IO.Unsafe (unsafePerformIO)

loadPicture :: String -> Picture
loadPicture path = unsafePerformIO $ loadBMP path

a :: Picture
a = loadPicture "images/a.bmp"


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
  where
    pic = loadPicture "images/seven.bmp"

eight :: Picture
eight = pic
  where
    pic = loadPicture "images/eight.bmp"

nine :: Picture
nine = pic
  where
    pic = loadPicture "images/nine.bmp"

zero :: Picture
zero = pic
  where
    pic = loadPicture "images/zero.bmp"


