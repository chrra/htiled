module Main where

import Data.Tiled.Load
import Data.Tiled.Types
import Test.Hspec
import Text.XML.HXT.Core

main = hspec tests

parseXml action xmlString =
  loadApply (readString [] xmlString) action

minimalTileX = "<tile id=\"0\" terrain=\"0,0,0,0\"/>"
minimalTile = Tile { tileId = 0
                   , tileProperties = []
                   , tileImage = Nothing
                   , tileObjectGroup = []
                   , tileAnimation = Nothing
                   }

tests = do
  describe "Data.Tiled.Load.tile" $ do
    it "parses a minimal tile definition" $
      parseXml tile minimalTileX `shouldReturn` minimalTile
