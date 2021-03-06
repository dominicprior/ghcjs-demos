{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.List
import Data.Ord
import Data.Maybe

main :: IO ()
main = do
  putStrLn "hello world"

data World = World {
    _blobs :: [Blob]
  , _worldSize :: Vec  -- in pixels.
  , _gravity :: Double -- in pixels per second per second.
} deriving (Show)

data Blob = Blob {
    _pos :: Vec     -- in pixels, from bottom left.
  , _vel :: Vec     -- in pixels per second.
  , _rad :: Double  -- in pixels.
  , _col :: String
} deriving (Show)

data Vec = Vec {
    _vecX :: Double
  , _vecY :: Double
} deriving (Show)

makeLenses ''World
makeLenses ''Blob
makeLenses ''Vec

type Time = Double
type BlobId = Int

data Collision = Collision {
    _collisionTime :: Time   -- how far in the future in seconds.
  , _who :: WhoIsColliding
} deriving (Show)

data WhoIsColliding = BlobBlob BlobId BlobId
                    | Side BlobId
                    | CeilingOrFloor BlobId
  deriving (Show)

-- Advances the world forward by the given time, taking into account
-- all the collisions that might happen in that time.

moveForwardsInTime :: Time -> World -> World
moveForwardsInTime t world =
  let (Collision dt who) = nextCollision world
      world' = moveBlobs (min t dt) world
  in if t < dt then world'
     else moveForwardsInTime (t - dt) $ over blobs (bounce who) world'

nextCollision :: World -> Collision
nextCollision world = minimumBy (comparing _collisionTime) $
  map (sideFn world) ids ++ map (upDownFn world) ids ++
    catMaybes [blobBlobFn world i j | i <- ids, j <- [i+1..n]]
  where n = length (_blobs world) - 1
        ids = [0..n]

-- The next collision of blob i with a side wall.

sideFn :: World -> BlobId -> Collision
sideFn world i =
  let b = _blobs world !! i
      u = _vecX $ _vel b
      x = _vecX $ _pos b
      r = _rad b
      w = _vecX $ _worldSize world
      t = if u > 0
          then (w - r - x) / u
          else (r - x) / u
  in Collision t $ Side i

-- The next collision of blob i with the ceiling or floor.

upDownFn :: World -> BlobId -> Collision
upDownFn world i =
  let b = _blobs world !! i
      v = _vecY $ _vel b
      y = _vecY $ _pos b
      r = _rad b
      g = _gravity world
      h = _vecY $ _worldSize world
      k = h - r - y
      discr = v*v - 2*g*k
      t = if v > 0 && discr > 0
          then (v - sqrt discr) / g
          else let k' = r - y
                   discr' = v*v - 2*g*k'
               in (v + sqrt discr') / g
  in Collision t $ CeilingOrFloor i

-- The next collision (if any) between blobs i and j.

blobBlobFn :: World -> BlobId -> BlobId -> Maybe Collision
blobBlobFn world i j =
  let a = _blobs world !! i
      b = _blobs world !! j
      v = _vel b ++- _vel a
      p = _pos b ++- _pos a
      r = _rad a + _rad b
      vv = dot v v
      pv = dot p v
      pp = dot p p
      discr = pv^2 - vv * (pp - r^2)
  in if discr > 0 && pv < 0 && pp > r^2
     then let t = (- pv - sqrt discr) / vv
          in Just $ Collision t $ BlobBlob i j
     else Nothing

-- Moves all the blobs forwards in time, assuming no collisions.

moveBlobs :: Time -> World -> World
moveBlobs t world = over blobs (map $ moveBlob (_gravity world) t) world

moveBlob :: Double -> Time -> Blob -> Blob
moveBlob g t (Blob (Vec x y) (Vec u v) r c) =
  Blob (Vec (x + u*t) (y + v*t - g*t*t/2))
       (Vec u (v - g*t))
       r c

-- Updates the blob list to account for the given collision.

bounce :: WhoIsColliding -> [Blob] -> [Blob]
bounce (Side i) bb           = over (element i . vel . vecX) negate bb
bounce (CeilingOrFloor i) bb = over (element i . vel . vecY) negate bb
bounce (BlobBlob i j) bb =
  let b = [bb!!i, bb!!j]
      masses     = map ((^2) . _rad) b
      velocities = map _vel b
      momentum = foldl1 (+++) $ zipWith (++*) velocities masses
      avVel = momentum ++/ sum masses
      relVels = map (++- avVel) velocities
      diff = _pos (bb!!j) ++- _pos (bb!!i)
      [v1,v2] = zipWith f velocities relVels
      f v rel = v ++- (along rel diff ++* 2)
  in set (element i . vel) v1 $ set (element j . vel) v2 bb


(+++) :: Vec -> Vec -> Vec
(Vec a b) +++ (Vec u v) = Vec (a+u) (b+v)
(++-) :: Vec -> Vec -> Vec
(Vec a b) ++- (Vec u v) = Vec (a-u) (b-v)
(++*) :: Vec -> Double -> Vec
(Vec u v) ++* k  = Vec (k*u) (k*v)
(++/) :: Vec -> Double -> Vec
v ++/ k  = v ++* (1/k)
len :: Vec -> Double
len v = sqrt $ dot v v
norm :: Vec -> Vec
norm v = v ++/ len v
dot :: Vec -> Vec -> Double
dot (Vec a b) (Vec x y) = a*x + b*y
along :: Vec -> Vec -> Vec
along x u = u ++* dot x u ++/ dot u u
