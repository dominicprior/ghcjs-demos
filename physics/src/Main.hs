module Main where

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
    vecX :: Double
  , vecY :: Double
}

type Time = Double
type BlobId = Int

data Collision = Collision {
    _collisionTime :: Time
  , _who :: WhoCollided
}

data WhoCollided = BlobBlob BlobId BlobId
                 | Side BlobId
                 | CeilingOrFloor BlobId
