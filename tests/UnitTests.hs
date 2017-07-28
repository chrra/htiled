{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.ByteString.Lazy.Char8 as BS
import           Data.String
import           Data.Text
import           Data.Tiled.Load
import           Data.Tiled.Types
import           Data.Word
import qualified Prelude
import           Prelude hiding (show)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Text.XML.Generator
import           Text.XML.HXT.Core hiding (trace, getName)

import           Debug.Trace

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

arbitraryTile :: Word32 -> Maybe Animation -> Gen Tile
arbitraryTile tileId tileAnimation = do
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

newtype ArbTilesetNoProperties =
  ArbTilesetNoProperties { getTilesetNoProperties :: Tileset }
  deriving Show

instance Arbitrary ArbTilesetNoProperties where
  arbitrary = ArbTilesetNoProperties <$> do
    ts <- getTileset <$> arbitrary
    return ts {tsProperties = []}

traceMap :: (Show b) => (a -> b) -> a -> a
traceMap f x = trace (show . f $ x) x

traceMapString :: (a -> String) -> a -> a
traceMapString f val = trace (f val) val

main = hspec tests

parseXml action xmlElems = Prelude.head <$>
  loadApply (readString [] (BS.unpack $ xrender xmlElems)) action

show :: (Show a, IsString s) => a -> s
show = fromString . Prelude.show

minimalTile = Tile { tileId = 0
                   , tileProperties = []
                   , tileImage = Nothing
                   , tileObjectGroup = []
                   , tileAnimation = Nothing
                   }

tileWithProperties = Tile { tileId = 0
                          , tileProperties = [("testProperty","testValue")]
                          , tileImage = Nothing
                          , tileObjectGroup = []
                          , tileAnimation = Nothing
                          }

tilesetToXml tileset =
  xelem "tileset" (tilesetAttrs,tilesetChildren)
  where
    tilesetAttrs = xattrs
      [ xattr "name" (fromString $ tsName tileset)
      , xattr "firstgid" (show . tsInitialGid $ tileset)
      , xattr "columns" (show . tsColumns $ tileset)
      , xattr "tilewidth" (show . tsTileWidth $ tileset)
      , xattr "tileheight" (show . tsTileHeight $ tileset)
      , xattr "tilecount" (show . tsTileCount $ tileset)
      ]
    tilesetChildren =
      [ tileToXml tile | tile <- tsTiles tileset ] ++
      images ++
      props
    images = imageElem <$> tsImages tileset
    imageElem img = xelem "image" $ xattrs
      [ xattr "source" (fromString . iSource $ img)
      , xattr "width" (show . iWidth $ img)
      , xattr "height" (show . iHeight $ img)
      ]
    props = if Prelude.null (tsProperties tileset)
            then []
            else [propertiesToXml $ tsProperties tileset]

xrenderString :: (Renderable r) => Xml r -> String
xrenderString = BS.unpack . xrender

propertiesToXml props =
  xelem "properties" $ xelems (fmap propertyToXml props)

propertyToXml (name,value) =
  xelem "property" $ xattrs
  [ xattr "name" (fromString name)
  , xattr "value" (fromString value)
  ]

tileToXml tile =
  xelem "tile" (tileAttrs,tileChildren)
  where
    tileAttrs = xattrs
      [ xattr "id" (show . tileId $ tile)
      ]
    tileChildren = xelems
      [ props
      ]
    props :: Xml Elem
    props = propertiesToXml (tileProperties tile)

tests = do
  describe "UnitTests.ArbName.arbitrary" $ do
    it "generates names" $ do
      property $ \ (ArbName name) -> Prelude.length name < 100
  describe "Data.Tiled.Load.tile" $ do
    it "parses a minimal tile definition" $
      parseXml tile (tileToXml minimalTile) `shouldReturn`
      minimalTile
    it "parses tiles with properties" $
      parseXml tile (tileToXml tileWithProperties) `shouldReturn`
      tileWithProperties
  describe "Data.Tiled.Load.properties" $ do
    it "parses empty Property lists" $ do
      parseXml properties (xelem "properties" ()) `shouldReturn`
        []
    it "parses property lists with one element" $ do
      let
        props = [("test","test")]
      parseXml properties (propertiesToXml props)
        `shouldReturn` props
  describe "Data.Tiled.Load.tileset" $ do
    it "parses tilesets without properties" $ do
      property $ \ (ArbTilesetNoProperties ts) -> monadicIO $ do
        parsedTs <- run (parseXml tileset (tilesetToXml ts))
        assert (parsedTs == ts)
    it "parses regular tilesets" $ do
      property $ \ (ArbTileset ts) -> monadicIO $
        run (parseXml tileset (tilesetToXml ts)) >>=
        assert . (\ result -> traceMap id result == traceMap id ts)
