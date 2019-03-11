module AI where

import Type (Zombie, Coords, zCoords)
import Structure.Object (zombieSize)

-- | Checks if Pea sees Zombie 
peaVision ::
  Coords -- ^ Coordinates of Pea's eyes
  -> Int
  -> Zombie -- ^ Zombie to check
  -> Bool -- ^ True if sees Flase otherwise
peaVision (_, y) screenBorder zombie = checkVision (zCoords zombie)
  where
    checkVision (zX, zY) = y > zY - zombieSize/2 && y < zY + zombieSize/2 && zX < fromIntegral screenBorder
