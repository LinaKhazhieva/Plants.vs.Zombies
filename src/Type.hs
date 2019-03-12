module Type where

import Structure.Object
import Graphics.Gloss

-- | Type for coordinates on the field
type Coords = (Float, Float)

-- | Data type to store different types of zombie
data ZombieType = Basic | Buckethead

-- | Data type for Zombie
data Zombie = Zombie
  { zType     :: ZombieType
  , zCoords   :: Coords     -- ^ coordinates of zombie
  , zDamage   :: Int        -- ^ damage the zombie received
  , zSeconds  :: Float      -- ^ seconds tha is left till zombie bite
  }

-- | Accessor for the speed of the zombies
zSpeed :: ZombieType -> Float
zSpeed Basic = 6
zSpeed Buckethead = 4

-- | Accessor of the zombies health 
zHealth :: ZombieType -> Int
zHealth Basic = 4
zHealth Buckethead = 6

-- | Accessor of the zombies strength
zStrength :: ZombieType -> Int
zStrength _z = 1

-- | Accessor to render zombie type
zPicture :: ZombieType -> Picture
zPicture Basic = basicZombie
zPicture Buckethead = bucketheadZombie

-- | One particular card
data Card = Card
  { cardColor  :: Color     -- ^ Color of Card
  , isActive   :: Bool      -- ^ is Card currently chosen
  , plantType  :: PlantType -- ^ type of Plant to plant if Card is active
  , cardCoords :: Coords    -- ^ Card coordinates
  }

-- | Data type to store different types of plant
data PlantType = PeasShooter

-- | Data type for Plants
data Plant = Plant
  { pType     :: PlantType
  , pCoords   :: Coords       -- ^ coordinates of plants
  , pDamage   :: Int          -- ^ damage the plant received
  , pBullet   :: [Projectile] -- ^ peas of the plant
  , pSeconds  :: Float        -- ^ seconds that is left till the plant shoots
  }

-- | Data type for projectile of the other plant
data Projectile = Projectile
  { prCoords :: Coords   -- ^ coordinates of projectile
  }

-- | Accessor for the plant
pHealth :: PlantType -> Int
pHealth _p = 10

-- | Accessor for the plant strength
pStrength :: PlantType -> Int 
pStrength _p = 1

-- | Accessor to render plant type
pPicture :: PlantType -> Picture
pPicture _p = plant

pCardColor :: PlantType -> Color
pCardColor PeasShooter = green

-- | Data type for Sunflowers,
-- stored in additional type, because sunflowers
-- has different dynamics
-- TODO: change the dynamic of the Projectile and
-- connect sunflowers to plant
data Sunflower = Sunflower
  { sCoords   :: Coords -- ^ coordinates of Sunflower
  , sDamage   :: Int    -- ^ health of the Sunflower 
  , sSun      :: [Sun]  -- ^ sun of the Sunflower
  , sSeconds  :: Float  -- ^ seconds that is left till the plant shoots
  }

-- | Data type for projectile of the sunflower
data Sun = Sun 
  { sunCoords :: Coords -- ^ coordinates of sun 
  }

-- | Accessor for the sunflower
sHealth :: Sunflower -> Int 
sHealth _s = 10 

-- | Data type for whole Universe
data Universe = Universe
  { uEnemies    :: [Zombie]    -- ^ predefined wave
  , uDefense    :: [Plant]     -- ^ list of plants that player put
  , uSunflowers :: [Sunflower] -- ^ list of sunflowers that player put
  , uCards      :: [Card]      -- ^ cards of plants to plant
  , uScreen     :: Picture     -- ^ special screen to denote the game over
  , uOver       :: Bool        -- ^ denotes if the game is over
  , uTime       :: Float       -- ^ amount of time passed since start
  }

