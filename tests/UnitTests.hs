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
import           Text.XML.Generator
import           Text.XML.HXT.Core

main = hspec tests

parseXml action xmlElems =
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
    tilesetChildren = [ tileToXml tile | tile <- tsTiles tileset ]

tileToXml tile =
  xelem "tile" (tileAttrs,tileChildren)
  where
    tileAttrs = xattrs
      [ xattr "id" (show . tileId $ tile)
      ]
    tileChildren = xelems
      [ xelem "properties" props
      ]
    props :: Xml Elem
    props = xelems
      [ propertyToXml name value
      | (name,value) <- tileProperties tile
      ]

propertyToXml n v = xelem "property" $ xattrs
  [ xattr "name" (fromString n)
  , xattr "value" (fromString v)
  ]

tests = do
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
      parseXml properties (xelem "properties" $ xelems
                           [ propertyToXml "test" "test"]) `shouldReturn`
        [("test","test")]
  -- describe "Data.Tiled.Load.tileset" $ do
  --   it "parses regular tilesets" $ do
  --     parseXml tileset examplesTilesetX `shouldReturn` exampleTileset
