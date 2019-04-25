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
