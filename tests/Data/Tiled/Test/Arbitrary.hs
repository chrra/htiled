{-# LANGUAGE RecordWildCards #-}

module Data.Tiled.Test.Arbitrary where

import Data.Word
import Test.QuickCheck

import Data.Tiled

newtype ArbName = ArbName { getName :: String } deriving Show

instance Arbitrary ArbName where
  arbitrary = ArbName <$> do
    (listOf $ oneof (return <$> ['a'..'z'])) `suchThat`
      ( \ l -> Prelude.length l < 100)

newtype ArbProperties = ArbProperties { getProperties :: Properties }
  deriving Show

instance Arbitrary ArbProperties where
  arbitrary =
    fmap ArbProperties . oneof . fmap return $ possibilities
    where
      possibilities =
        [ []
        , [("propName","propValue")]
        , [ ("propName","propValue")
          , ("otherName","otherValue")
          ]
        ]

arbMaybe :: Gen a -> Gen (Maybe a)
arbMaybe gen = arbitrary >>= \ isNothing ->
  if isNothing then return Nothing else Just <$> gen

arbitraryTile :: Word32 -> Maybe Animation -> Gen Tile
arbitraryTile tileId tileAnimation = do
  tileImage <- arbMaybe $ arbitraryImage 10 10
  tileProperties <- getProperties <$> arbitrary
  return Tile {..}
  where
    tileImage = Nothing
    tileObjectGroup = []

arbitraryImage iWidth iHeight = do
  return Image {..}
  where
    iSource = ""
    iTrans = Nothing

arbitraryTileset tsInitialGid = do
  tsName <- getName <$> arbitrary
  tsColumns <- (getPositive <$> arbitrary) `suchThat` (<= 10)
  tsLines <-
    (getPositive <$> arbitrary) `suchThat` (<= 10) :: Gen Int
  let
    tsTileCount = tsLines * tsColumns
  tsProperties <- getProperties <$> arbitrary
  tsTiles <- mapM
    (flip arbitraryTile Nothing)
    [0..(fromIntegral $ tsTileCount - 1)]
  tsImages <- (:[]) <$> arbitraryImage
    (tsTileWidth*tsColumns)
    (tsTileHeight*tsLines)
  return Tileset {..}
  where
    tsMargin = 0
    tsSpacing = 0
    tsTileWidth = 5
    tsTileHeight = 5

newtype ArbTileset = ArbTileset { getTileset :: Tileset }
  deriving Show

instance Arbitrary ArbTileset where
  arbitrary = ArbTileset <$> arbitraryTileset 0

newtype ArbTile = ArbTile { getTile :: Tile }
  deriving Show

instance Arbitrary ArbTile where
  arbitrary = ArbTile <$>
    (flip arbitraryTile Nothing =<< (getPositive <$> arbitrary))

newtype ArbTilesetNoProperties =
  ArbTilesetNoProperties { getTilesetNoProperties :: Tileset }
  deriving Show

instance Arbitrary ArbTilesetNoProperties where
  arbitrary = ArbTilesetNoProperties <$> do
    ts <- getTileset <$> arbitrary
    return ts {tsProperties = []}

