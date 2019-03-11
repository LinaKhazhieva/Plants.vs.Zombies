module AI where

import Type (Zombie, Coords, zCoords)
import Structure.Object (zombieSize)

-- | Checks if Pea sees Zombie 
peaVision ::
  Coords -- ^ Coordinates of Pea's eyes
  -> Zombie -- ^ Zombie to check
  -> Bool -- ^ True if sees Flase otherwise
peaVision (_, y) zombie = checkVision (zCoords zombie)
  where
    checkVision (_, zY) = y > zY - zombieSize/2 && y < zY + zombieSize/2
