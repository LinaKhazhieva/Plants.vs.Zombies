{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}

module Structure.Alphabet where

import           Data.Maybe
import           Graphics.Gloss
import           System.IO.Unsafe (unsafePerformIO)

-- | Function to load a picture from given path 
loadPicture :: String -> Picture
loadPicture path = unsafePerformIO $ loadBMP path

-- | Function to load picture from a given char character
charPicture :: Char -> Picture
charPicture c = loadPicture ("images/" ++ [c] ++ ".bmp")

-- | Function to load all pictures of all chars
loadCharPictures :: [Char] -> Char -> Picture
loadCharPictures cs c = fromMaybe blank (lookup c m)
  where
    m = zip cs (map charPicture cs)

-- | Function to map all the pictures of chars to the chars themselves
defaultCharPictures :: Char -> Picture
defaultCharPictures = loadCharPictures (['a'..'z'] ++ ['0'..'9'])

-- | Function to choose the characters from the picture 
strPicture :: String -> Picture
strPicture s = pictures (zipWith drawCharAt [0..] s)
  where
    drawCharAt i c = translate (d * fromInteger i) 0 (defaultCharPictures c)
    d = 12

-- | Translate the number to the picture
numPicture :: Int -> Picture
numPicture = strPicture . show
