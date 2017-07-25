{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Tiled.Load
import Data.Tiled.Types
import Test.Hspec
import Text.XML.HXT.Core
import Text.XML.Generator
import Data.ByteString.Lazy.Char8

main = hspec tests

parseXml action xmlElems =
  loadApply (readString [] (unpack $ xrender xmlElems)) action

minimalTileX = xelem "tile" (xattr "id" "0")
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
tileWithPropertiesX = xelem "tile" (xattr "id" "0", properties)
  where
    properties = xelem "properties" . xelems $
      [ xelem "property" . xattrs $
        [ xattr "name" "testProperty"
        , xattr "value" "testValue"
        ]
      ]

tests = do
  describe "Data.Tiled.Load.tile" $ do
    it "parses a minimal tile definition" $
      parseXml tile minimalTileX `shouldReturn` minimalTile
    it "parses tiles with properties" $
      parseXml tile tileWithPropertiesX `shouldReturn` tileWithProperties
