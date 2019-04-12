{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}

module Structure.Alphabet where

import           Data.Maybe
import           Graphics.Gloss
import           System.IO.Unsafe (unsafePerformIO)

loadPicture :: String -> Picture
loadPicture path = unsafePerformIO $ loadBMP path

loadCharPictures :: [Char] -> Char -> Picture
loadCharPictures cs c = fromMaybe blank (lookup c m)
  where
    m = zip cs (map charPicture cs)

defaultCharPictures :: Char -> Picture
defaultCharPictures = loadCharPictures (['a'..'z'] ++ ['0'..'9'])

charPicture :: Char -> Picture
charPicture c = loadPicture ("images/" ++ [c] ++ ".bmp")

strPicture :: String -> Picture
strPicture s = pictures (zipWith drawCharAt [0..] s)
  where
    drawCharAt i c = translate (d * fromInteger i) 0 (defaultCharPictures c)
    d = 12

numPicture :: Int -> Picture
numPicture = strPicture . show


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


