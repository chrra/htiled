{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.ByteString.Lazy.Char8 as BS
import           Data.String
import           Data.Text
import           Data.Word
import qualified Prelude
import           Prelude hiding (show)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Text.XML.Generator
import           Text.XML.HXT.Core hiding (trace, getName)

import           Data.Tiled.Load
import           Data.Tiled.Test.Arbitrary
import           Data.Tiled.Test.Xml
import           Data.Tiled.Types

import           Debug.Trace

traceMap :: (Show b) => (a -> b) -> a -> a
traceMap f x = trace (show . f $ x) x

traceMapString :: (a -> String) -> a -> a
traceMapString f val = trace (f val) val

main = hspec tests

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
    it "parses any tiled" $ do
      property $ \ (ArbTile t) -> monadicIO $
        run (parseXml tile (tileToXml t)) >>=
        assert . (== t) . traceMap id
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
        assert . (== ts)
