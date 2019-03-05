{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}

module Type where

-- | Type for coordinates on the field
type Coords = (Float, Float)

-- | Data type for Zombie
data Zombie = Zombie
  { zCoords   :: Coords    -- ^ coordinates of zombie
  , zSpeed    ::  Float    -- ^ movement speed
  , zStrength ::    Int    -- ^ strength of the zombie
  }

-- | Data type for Plants
data Plant = Plant
  { pCoords   ::     Coords -- ^ coordinates of plants
  , pHealth   ::        Int -- ^ health of the plant
--  , pStrength ::        Int -- ^ strength of the plant
  , pBullet   :: Projectile -- ^ projectile of the plant
  }

data Projectile = Projectile
  { prX :: Float            -- ^ coordinates of projectile
  }

-- | Data type for whole Universe
data Universe = Universe
  { uEnemies :: [Zombie]   -- ^ list of enemies
  , uDefense ::  [Plant]   -- ^ list of plants
  , uTime    ::    Float   -- ^ amount of time passed since start
  }
