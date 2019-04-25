{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}

module Type where

import           Graphics.Gloss
import           Structure.Alphabet
import           Structure.Object

-- | Type for coordinates on the field
type Coords = (Float, Float)
type UserName = [Char]

-- | Data type to store different types of zombie
data ZombieType = Basic | Buckethead
  deriving (Show, Read)

-- | Data type for Zombie
data Zombie = Zombie
  { zType    :: ZombieType
  , zCoords  :: Coords     -- ^ coordinates of zombie
  , zDamage  :: Int        -- ^ damage the zombie received
  , zSeconds :: Float      -- ^ seconds tha is left till zombie bite
  }
  deriving (Show, Read)

-- | Accessor for the speed of the zombies
zSpeed :: ZombieType -> Float
zSpeed Basic      = 10
zSpeed Buckethead = 10

-- | Accessor of the zombies health
zHealth :: ZombieType -> Int
zHealth Basic      = 200
zHealth Buckethead = 1300

-- | Accessor of the zombies strength
zStrength :: ZombieType -> Int
zStrength _z = 100

-- | Accessor to render zombie type
zPicture :: ZombieType -> Picture
zPicture Basic      = basicZombie
zPicture Buckethead = bucketheadZombie

-- | One particular card
data Card = Card
  { isActive  :: Bool      -- ^ is Card currently chosen
  , plantType :: PlantType -- ^ type of Plant to plant if Card is active
  , cCoords   :: Coords    -- ^ Card coordinates
  , cTime     :: Float
  }
  deriving (Show, Read)

cPicture :: PlantType -> Picture
cPicture PeasShooter = peasshooterCard
cPicture Sunflower   = sunflowerCard

cMoney :: PlantType -> Int
cMoney PeasShooter = 100
cMoney Sunflower   = 50

cFrequency :: PlantType -> Float
cFrequency PeasShooter = 5
cFrequency Sunflower   = 5

-- | Data type to store different types of plant
data PlantType = PeasShooter | Sunflower
  deriving (Eq, Show, Read)

-- | Data type for Plants
data Plant = Plant
  { pType    :: PlantType
  , pCoords  :: Coords       -- ^ coordinates of plants
  , pDamage  :: Int          -- ^ damage the plant received
  , pBullet  :: [Projectile] -- ^ peas of the plant
  , pSeconds :: Float        -- ^ seconds that is left till the plant shoots
  }
  deriving (Show, Read)

data ProjectileType = Sun | Pea
  deriving (Eq, Show, Read)

-- | Data type for projectile of the other plant
data Projectile = Projectile
  { prType   :: ProjectileType
  , prCoords :: Coords          -- ^ coordinates of projectile
  }
  deriving (Show, Read)

-- | Accessor for the plant
pHealth :: PlantType -> Int
pHealth PeasShooter = 300
pHealth Sunflower   = 300

-- | Accessor for the plant strength
pStrength :: PlantType -> Int
pStrength PeasShooter = 20
pStrength Sunflower   = 0

-- | Accessor to render plant type
pPicture :: PlantType -> Picture
pPicture PeasShooter = plant
pPicture Sunflower   = sunflower

pFrequency :: PlantType -> Float
pFrequency PeasShooter = 1.5
pFrequency Sunflower   = 24

pStarterTimer :: PlantType -> Float
pStarterTimer PeasShooter = 0
pStarterTimer Sunflower   = 7

prPicture :: ProjectileType -> Picture
prPicture Sun = sun
prPicture Pea = projectile

-- | Data type for whole Universe
data Universe = Universe
  { uEnemies  :: [Zombie]              -- ^ predefined wave
  , uDefense  :: [Plant]               -- ^ list of plants that player put
  , uCards    :: [Card]                -- ^ cards of plants to plant
  , uSuns     :: ([Projectile], Float) -- ^ suns falling from the sky,
                                         -- with time left to create the sun 
  , uOver     :: Bool                  -- ^ denotes if the game is over
  , uTime     :: Float                 -- ^ amount of time passed since start
  , uMoney    :: Int
  , uLevelNum :: Int
  , uStage    :: Int
  }
  deriving (Show, Read)

newScreen :: Int -> Int -> Picture
newScreen _ 1 = sunflowerCard
newScreen _ 2 = levelOne
newScreen _ 3 = menu 
newScreen _ 4 = menu <> user 
newScreen _ _ = blank



-- | TODO: change to bool
data EditType = Rename | None | OK
  deriving (Eq, Show, Read)

data State = State
  { sName     :: UserName 
  , sEdit     :: EditType
  , sUniverse :: Universe
  , sLevels   :: [Universe]
  }
  deriving (Show, Read)
