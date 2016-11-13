{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens

main :: IO ()
main = do
  putStrLn "hello world"

data World = World {
    _blobs :: [Blob]
  , _worldSize :: Vec  -- in pixels.
  , _gravity :: Double -- in pixels per second per second.
}

data Blob = Blob {
    _pos :: Vec     -- in pixels, from bottom left.
  , _vel :: Vec     -- in pixels per second.
  , _rad :: Double  -- in pixels.
  , _col :: String
}

data Vec = Vec {
    _vecX :: Double
  , _vecY :: Double
}

makeLenses ''World
makeLenses ''Blob
makeLenses ''Vec

type Time = Double
type BlobId = Int

data Collision = Collision {
    _collisionTime :: Time   -- how far in the future in seconds.
  , _who :: WhoIsColliding
}

data WhoIsColliding = BlobBlob BlobId BlobId
                    | Side BlobId
                    | CeilingOrFloor BlobId

moveForwardsInTime :: Time -> World -> World
moveForwardsInTime t world =
  let (Collision dt who) = nextCollision world
      world' = moveBlobs (min t dt) world
  in if t < dt then world'
     else moveForwardsInTime (t - dt) $ bounce who world'

nextCollision :: World -> Collision
nextCollision = undefined

moveBlobs :: Time -> World -> World
moveBlobs = undefined

bounce :: WhoIsColliding -> World -> World
bounce = over blobs . bounce'

bounce' :: WhoIsColliding -> [Blob] -> [Blob]
bounce' (Side i) bb           = over (element i . vel . vecX) negate bb
bounce' (CeilingOrFloor i) bb = over (element i . vel . vecY) negate bb
bounce' (BlobBlob i j) bb =
  let b = [bb!!i, bb!!j]
      masses     = map ((^2) . _rad) b
      velocities = map _vel b
      momentum = foldl1 (+++) $ zipWith (++*) velocities masses
      avVel = momentum ++/ sum masses
      relVels = map (++- avVel) velocities
      diff = _pos (bb!!j) ++- _pos (bb!!i)
      [v1,v2] = zipWith f velocities relVels
      f v rel = v ++- along rel diff
  in set (element i . vel) v1 $ set (element j . vel) v2 bb


(+++) :: Vec -> Vec -> Vec
(Vec a b) +++ (Vec u v) = Vec (a+u) (b+v)
(++-) :: Vec -> Vec -> Vec
(Vec a b) ++- (Vec u v) = Vec (a-u) (b-v)
(++*) :: Vec -> Double -> Vec
(Vec u v) ++* k  = Vec (k*u) (k*v)
(++/) :: Vec -> Double -> Vec
(Vec u v) ++/ k  = Vec (k/u) (k/v)
len :: Vec -> Double
len v = sqrt $ dot v v
norm :: Vec -> Vec
norm v = v ++/ len v
dot :: Vec -> Vec -> Double
dot (Vec a b) (Vec x y) = a*x + b*y
along :: Vec -> Vec -> Vec
along x u = u ++* dot x u ++/ dot u u
