module Type where

import Graphics.Gloss.Data.Color (Color)

-- | Type for coordinates on the field
type Coords = (Float, Float)

-- | Data type to store different types of zombie
data ZombieType = ZombieOne | ZombieTwo | ZombieThree

-- | Data type to store different types of plant
data PlantType = PlantOne | PlantTwo

-- | Data type for Zombie
data Zombie = Zombie
  { zType     :: ZombieType
  , zCoords   :: Coords    -- ^ coordinates of zombie
  , zDamage   :: Int -- ^ heatlth of the zombie 
  }

-- | One particular card
data Card = Card {
  cardColor :: Color, -- ^ Color of Card 
  isActive :: Bool, -- ^ is Card currently chosen
  plantType :: PlantType, -- ^ type of Plant to plant if Card is active
  cardCoords :: Coords -- ^ Card coordinates
}

-- | Data type for Plants
data Plant = Plant
  { pType     ::  PlantType
  , pCoords   ::     Coords -- ^ coordinates of plants
  , pDamage   ::        Int -- ^ health of the plant
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
  , uCards   ::   [Card]   -- ^ cards of plants to plant  
  }
