module Type where

import Structure.Object
import Structure.Alphabet
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
zSpeed Basic = 10
zSpeed Buckethead = 10

-- | Accessor of the zombies health 
zHealth :: ZombieType -> Int
zHealth Basic = 200
zHealth Buckethead = 1300

-- | Accessor of the zombies strength
zStrength :: ZombieType -> Int
zStrength _z = 100

-- | Accessor to render zombie type
zPicture :: ZombieType -> Picture
zPicture Basic = basicZombie
zPicture Buckethead = bucketheadZombie

-- | One particular card
data Card = Card
  { isActive   :: Bool      -- ^ is Card currently chosen
  , plantType  :: PlantType -- ^ type of Plant to plant if Card is active
  , cCoords    :: Coords    -- ^ Card coordinates
  , cTime      :: Float
  }

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

digitToPic :: Int -> Picture
digitToPic 0 = zero
digitToPic 1 = one
digitToPic 2 = two
digitToPic 3 = three
digitToPic 4 = four
digitToPic 5 = five
digitToPic 6 = six
digitToPic 7 = seven
digitToPic 8 = eight
digitToPic 9 = nine
digitToPic _ = blank

-- | Data type for whole Universe
data Universe = Universe
  { uEnemies    :: [Zombie]              -- ^ predefined wave
  , uDefense    :: [Plant]               -- ^ list of plants that player put
  , uCards      :: [Card]                -- ^ cards of plants to plant
  , uSuns       :: ([Projectile], Float) -- ^ suns falling from the sky,
                                         -- with time left to create the sun
  , uScreen     :: Picture               -- ^ special screen to denote
                                         -- the game over
  , uOver       :: Bool                  -- ^ denotes if the game is over
  , uTime       :: Float                 -- ^ amount of time passed since start
  , uMoney      :: Int
  , uLevelNum   :: Int
  , uStage      :: Int                 
  }

newScreen :: Int -> Int -> Picture
newScreen _ 1 = sunflowerCard
newScreen _ 2 = levelOne
newScreen _ 3 = menu
newScreen _ 4 = menu <> user <> a
newScreen _ _ = blank

data State = State Universe [Universe]
