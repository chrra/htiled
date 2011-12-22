{-# LANGUAGE Arrows, UnicodeSyntax, RecordWildCards #-}
module Data.Tiled.Load (loadMapFile) where

import Prelude hiding ((.), id)
import Control.Category ((.), id)
import Data.Bits (testBit, clearBit)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Map (fromDistinctAscList, Map)
import Data.Maybe (listToMaybe, fromMaybe, isNothing)
import Data.Word (Word32)

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import Text.XML.HXT.Core

import Data.Tiled.Types

-- | Load a map file.
loadMapFile ∷ FilePath → IO TiledMap
loadMapFile fp = head `fmap` runX (
        configSysVars [withValidate no, withWarnings yes]
    >>> readDocument [] fp
    >>> getChildren >>> isElem
    >>> doMap fp)

properties ∷ IOSArrow XmlTree Properties
properties = listA $ getChildren >>> isElem >>> hasName "properties"
            >>> getChildren >>> isElem >>> hasName "property"
            >>> getAttrValue "name" &&& getAttrValue "value"

getAttrR ∷ (Read α, Num α) ⇒ String → IOSArrow XmlTree α
getAttrR a = arr read . getAttrValue0 a

doMap ∷ FilePath → IOSArrow XmlTree TiledMap
doMap mapPath = proc m → do
    mapOrientation ← arr (\x → case x of "orthogonal" → Orthogonal
                                         "isometric" → Isometric
                                         _ → error "unsupported orientation")
                     . getAttrValue "orientation" ⤙ m
    mapWidth       ← getAttrR "width"      ⤙ m
    mapHeight      ← getAttrR "height"     ⤙ m
    mapTileWidth   ← getAttrR "tilewidth"  ⤙ m
    mapTileHeight  ← getAttrR "tileheight" ⤙ m
    mapProperties  ← properties            ⤙ m
    mapTilesets    ← tilesets              ⤙ m
    mapLayers      ← layers                ⤙ (m, (mapWidth, mapHeight))
    returnA        ⤙ TiledMap {..}

layers ∷ IOSArrow (XmlTree, (Int, Int)) [Layer]
layers = listA (first (getChildren >>> isElem) >>> doObjectGroup <+> doLayer)
  where
    doObjectGroup = arr fst >>> hasName "objectgroup" >>> id &&& (listA object >>> arr Right) >>> common
    
    object = getChildren >>> isElem >>> hasName "object"
         >>> proc obj → do
        objectName   ← arr listToMaybe . listA (getAttrValue "name") ⤙ obj
        objectType   ← arr listToMaybe . listA (getAttrValue "type") ⤙ obj
        objectX      ← getAttrR "x"                                  ⤙ obj
        objectY      ← getAttrR "y"                                  ⤙ obj
        objectWidth  ← arr listToMaybe . listA (getAttrR "width")    ⤙ obj
        objectHeight ← arr listToMaybe . listA (getAttrR "height")   ⤙ obj
        objectGid    ← arr listToMaybe . listA (getAttrR "gid")      ⤙ obj
        objectProperties ← properties                                ⤙ obj
        returnA      ⤙ Object {..}
    
    doLayer = first (hasName "layer") >>> arr fst &&& (doData >>> arr Left) >>> common

    doData = first (getChildren >>> isElem >>> hasName "data")
         >>> proc (dat, (w, h)) → do
                encoding    ← getAttrValue "encoding"        ⤙ dat
                compression ← getAttrValue "compression"     ⤙ dat
                text        ← getText . isText . getChildren ⤙ dat
                returnA ⤙ dataToTiles w h encoding compression text

    -- Width → Height → Encoding → Compression → Data → [Tile]
    dataToTiles ∷ Int → Int → String → String → String → Map (Int, Int) Tile
    dataToTiles w h "base64" "gzip" = toMap w h . base64 GZip.decompress
    dataToTiles w h "base64" "zlib" = toMap w h . base64 Zlib.decompress
    dataToTiles _ _ _ _ = error "unsupported tile data format, only base64 and \
                                \gzip/zlib is supported at the moment."

    toMap w h = fromDistinctAscList . sort . filter (\(_, x) → tileGid x /= 0)
                . zip [(x, y) | y ← [0..h-1], x ← [0..w-1]]

    base64 f = bytesToTiles . LBS.unpack . f . LBS.fromChunks
                            . (:[]) . B64.decodeLenient . BS.pack

    bytesToTiles (a:b:c:d:xs) = Tile { .. } : bytesToTiles xs
      where n = f a + f b * 256 + f c * 65536 + f d * 16777216
            f = fromIntegral . fromEnum ∷ Char → Word32
            tileGid = n `clearBit` 30 `clearBit` 31
            tileIsVFlipped = n `testBit` 30
            tileIsHFlipped = n `testBit` 31
    bytesToTiles [] = []
    bytesToTiles _ = error "number of bytes not a multiple of 4."

    common = proc (l, x) → do
        layerName       ← getAttrValue "name"              ⤙ l
        layerOpacity    ← arr (fromMaybe 1 . listToMaybe)
                          . listA (getAttrR "opacity")     ⤙ l
        layerIsVisible  ← arr (isNothing . listToMaybe)
                          . listA (getAttrValue "visible") ⤙ l
        layerProperties ← properties                      ⤙ l
        returnA ⤙ case x of Left  layerData    → Layer {..}
                            Right layerObjects → ObjectLayer {..}


tilesets ∷ IOSArrow XmlTree [Tileset]
tilesets = listA $ getChildren >>> isElem >>> hasName "tileset"
         >>> proc ts → do
              tsName        ← getAttrValue "name"     ⤙ ts
              tsInitialGid  ← getAttrR "firstgid"     ⤙ ts
              tsTileWidth   ← getAttrR "tilewidth"    ⤙ ts
              tsTileHeight  ← getAttrR "tileheight"   ⤙ ts
              tsImages      ← images                  ⤙ ts
              tsTileProperties ← listA tileProperties ⤙ ts
              returnA ⤙ Tileset {..}
  where tileProperties ∷ IOSArrow XmlTree (Word32, Properties)
        tileProperties = getChildren >>> isElem >>> hasName "tile"
                     >>> getAttrR "id" &&& properties

        images = listA (getChildren >>> isElem >>> hasName "image" >>>
                         proc image → do
                              iSource ← getAttrValue "source"   ⤙ image
                              iTrans  ← arr (fmap colorToTriplet . listToMaybe)
                                            . listA (getAttrValue0 "trans") ⤙ image
                              iWidth  ← getAttrR "width"        ⤙ image
                              iHeight ← getAttrR "height"       ⤙ image
                              returnA ⤙ Image {..})

        colorToTriplet x = (h x, h $ drop 2 x, h $ drop 4 x)
          where h (y:z:_) = fromIntegral $ digitToInt y * 16 + digitToInt z
                h _ = error "invalid color in an <image ...> somewhere."
