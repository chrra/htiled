module Data.Tiled
    ( module X
    , module Data.Tiled
    ) where

import           Data.Tiled.Load  as X
import           Data.Tiled.Types as X
import           Data.Vector      (Vector, force, slice)

-- | Yield a slice of the tile vectors without copying it.
--
-- The vectors must be at least x+w wide and y+h tall.
sliceTileVectors
  :: Int -- ^ X
  -> Int -- ^ Y
  -> Int -- ^ Width
  -> Int -- ^ Height
  -> Vector (Vector (Maybe TileIndex))
  -> Vector (Vector (Maybe TileIndex))
sliceTileVectors x y w h = (slice x w <$>) . slice y h

-- | Yield the argument but force it not to retain any extra memory,
-- possibly by copying it.
--
-- Useful after slicing huge tile vectors.
forceTileVectors
  :: Vector (Vector (Maybe TileIndex))
  -> Vector (Vector (Maybe TileIndex))
forceTileVectors = force . fmap force
