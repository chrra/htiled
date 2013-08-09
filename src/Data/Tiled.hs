{-# LANGUAGE UnicodeSyntax #-}
module Data.Tiled
    ( module Data.Tiled.Load
    , module Data.Tiled.Types
    , module Data.Tiled
    ) where

import Data.Vector

import Data.Tiled.Load
import Data.Tiled.Types

-- | Yield a slice of the tile vectors without copying it.
--
-- The vectors must be at least x+w wide and y+h tall.
sliceTileVectors ∷ Int -- ^ X
                 → Int -- ^ Y
                 → Int -- ^ Width
                 → Int -- ^ Height
                 → Vector (Vector (Maybe Tile))
                 → Vector (Vector (Maybe Tile))
sliceTileVectors x y w h v = fmap (slice x w) $ slice y h v

-- | Yield the argument but force it not to retain any extra memory,
-- possibly by copying it.
--
-- Useful after slicing huge tile vectors.
forceTileVectors ∷ Vector (Vector (Maybe Tile))
                 → Vector (Vector (Maybe Tile))
forceTileVectors = force . fmap force
