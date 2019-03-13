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
  { isActive   :: Bool      -- ^ is Card currently chosen
  , plantType  :: PlantType -- ^ type of Plant to plant if Card is active
  , cCoords :: Coords    -- ^ Card coordinates
  }

cPicture :: PlantType -> Picture
cPicture PeasShooter = peasshooterCard
cPicture Sunflower   = sunflowerCard

-- | Data type to store different types of plant
data PlantType = PeasShooter | Sunflower
  deriving (Eq)

-- | Data type for Plants
data Plant = Plant
  { pType     :: PlantType
  , pCoords   :: Coords       -- ^ coordinates of plants
  , pDamage   :: Int          -- ^ damage the plant received
  , pBullet   :: [Projectile] -- ^ peas of the plant
  , pSeconds  :: Float        -- ^ seconds that is left till the plant shoots
  }

data ProjectileType = Sun | Pea
  deriving (Eq)

-- | Data type for projectile of the other plant
data Projectile = Projectile
  { prType   :: ProjectileType
  , prCoords :: Coords          -- ^ coordinates of projectile
  }

-- | Accessor for the plant
pHealth :: PlantType -> Int
pHealth _p = 1

-- | Accessor for the plant strength
pStrength :: PlantType -> Int 
pStrength PeasShooter = 1
pStrength Sunflower   = 0

-- | Accessor to render plant type
pPicture :: PlantType -> Picture
pPicture PeasShooter = plant
pPicture Sunflower   = sunflower

pFrequency :: PlantType -> Float
pFrequency PeasShooter = 5
pFrequency Sunflower   = 4

pStarterTimer :: PlantType -> Float
pStarterTimer PeasShooter = 0
pStarterTimer Sunflower   = pFrequency Sunflower

prPicture :: ProjectileType -> Picture
prPicture Sun = sun
prPicture Pea = projectile

-- | Data type for whole Universe
data Universe = Universe
  { uEnemies    :: [Zombie]    -- ^ predefined wave
  , uDefense    :: [Plant]     -- ^ list of plants that player put
  , uCards      :: [Card]      -- ^ cards of plants to plant
  , uScreen     :: Picture     -- ^ special screen to denote the game over
  , uOver       :: Bool        -- ^ denotes if the game is over
  , uTime       :: Float       -- ^ amount of time passed since start
  }

