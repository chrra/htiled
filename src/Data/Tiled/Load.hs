{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax   #-}
module Data.Tiled.Load (loadMapFile, loadMap) where

import           Control.Category           (id, (.))
import           Data.Bits                  (clearBit, testBit)
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char                  (digitToInt)
import           Data.List                  (sort)
import           Data.List.Split            (splitOn)
import           Data.Map                   (Map, fromDistinctAscList)
import           Data.Maybe                 (fromMaybe, isNothing, listToMaybe)
import           Data.Word                  (Word32)
import           Prelude                    hiding (id, (.))

import qualified Codec.Compression.GZip     as GZip
import qualified Codec.Compression.Zlib     as Zlib
import           System.FilePath            (dropFileName, (</>))
import           Text.XML.HXT.Core

import           Data.Tiled.Types

-- | Load a map from a string
loadMap ∷ String → IO TiledMap
loadMap str = load (readString [] str) "binary"

-- | Load a map file.
loadMapFile ∷ FilePath → IO TiledMap
loadMapFile fp = load (readDocument [] fp) fp

load ∷ IOStateArrow () XmlTree XmlTree -> FilePath -> IO TiledMap
load a fp = head `fmap` runX (
        configSysVars [withValidate no, withWarnings yes]
    >>> a
    >>> getChildren >>> isElem
    >>> doMap fp)

properties ∷ IOSArrow XmlTree Properties
properties = listA $ getChildren >>> isElem >>> hasName "properties"
            >>> getChildren >>> isElem >>> hasName "property"
            >>> getAttrValue "name" &&& getAttrValue "value"

getAttrR ∷ (Read α, Num α) ⇒ String → IOSArrow XmlTree α
getAttrR a = arr read . getAttrValue0 a

getAttrMaybe ∷ (Read α, Num α) ⇒ String → IOSArrow XmlTree (Maybe α)
getAttrMaybe a = arr tm . getAttrValue a
    where
        tm "" = Nothing
        tm s = Just $ read s

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
    mapTilesets    ← tilesets mapPath      ⤙ m
    mapLayers      ← layers                ⤙ (m, (mapWidth, mapHeight))
    returnA        ⤙ TiledMap {..}

layers ∷ IOSArrow (XmlTree, (Int, Int)) [Layer]
layers = listA (first (getChildren >>> isElem) >>> doObjectGroup <+> doLayer <+> doImageLayer)
  where
    doObjectGroup = arr fst >>> hasName "objectgroup" >>> id &&& (listA object >>> arr Right) >>> common

    object = getChildren >>> isElem >>> hasName "object"
         >>> proc obj → do
        objectName     ← arr listToMaybe . listA (getAttrValue "name") ⤙ obj
        objectType     ← arr listToMaybe . listA (getAttrValue "type") ⤙ obj
        objectX        ← getAttrR "x"                                  ⤙ obj
        objectY        ← getAttrR "y"                                  ⤙ obj
        objectWidth    ← arr listToMaybe . listA (getAttrR "width")    ⤙ obj
        objectHeight   ← arr listToMaybe . listA (getAttrR "height")   ⤙ obj
        objectGid      ← arr listToMaybe . listA (getAttrR "gid")      ⤙ obj
        objectPolygon  ← arr listToMaybe . polygon                     ⤙ obj
        objectPolyline ← arr listToMaybe . polyline                    ⤙ obj
        objectProperties ← properties                                  ⤙ obj
        returnA      ⤙ Object {..}

    polygon ∷ IOSArrow XmlTree [Polygon]
    polygon = listA $ getChildren >>> isElem >>> hasName "polygon"
          >>> getAttrValue "points" >>> arr (Polygon . points)
    polyline ∷ IOSArrow XmlTree [Polyline]
    polyline = listA $ getChildren >>> isElem >>> hasName "polyline"
          >>> getAttrValue "points" >>> arr (Polyline . points)

    points :: String → [(Int, Int)]
    points s = (x, y):if null rest then [] else points rest
        where (p, rest) = drop 1 `fmap` break (==' ') s
              (x', y') = drop 1 `fmap` break (==',') p
              x = read x'
              y = read y'

    doImageLayer = arr fst >>> hasName "imagelayer" >>> id &&& image >>> proc (l, layerImage) → do
        layerName ← getAttrValue "name" ⤙ l
        layerOpacity ← arr (fromMaybe 1 . listToMaybe) . listA (getAttrR "opacity") ⤙ l
        layerIsVisible ← arr (isNothing . listToMaybe) . listA (getAttrValue "visible") ⤙ l
        layerProperties ← properties ⤙ l
        returnA ⤙ ImageLayer{..}

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
    dataToTiles w h "csv"    _      = toMap w h . csv
    dataToTiles _ _ _ _ = error "unsupported tile data format, only base64 with \
                                \gzip/zlib and csv are supported at the moment."

    toMap w h = fromDistinctAscList . sort . filter (\(_, x) → tileGid x /= 0)
                . zip [(x, y) | y ← [0..h-1], x ← [0..w-1]]

    base64 f = wordsToTiles . bytesToWords . LBS.unpack . f . LBS.fromChunks
                            . (:[]) . B64.decodeLenient . BS.pack

    csv = wordsToTiles . map (read ∷ String → Word32)
                       . splitOn ","
                       . filter (`elem` (',':['0' .. '9']))

    bytesToWords []           = []
    bytesToWords (a:b:c:d:xs) = n : bytesToWords xs
      where n = f a + f b * 256 + f c * 65536 + f d * 16777216
            f = fromIntegral . fromEnum ∷ Char → Word32
    bytesToWords _            = error "number of bytes not a multiple of 4."

    wordsToTiles []           = []
    wordsToTiles (w:ws)       = Tile { .. } : wordsToTiles ws
      where tileGid           = w `clearBit` 30 `clearBit` 31 `clearBit` 29
            tileIsVFlipped    = w `testBit` 30
            tileIsHFlipped    = w `testBit` 31
            tileIsDiagFlipped = w `testBit` 29

    common = proc (l, x) → do
        layerName       ← getAttrValue "name"              ⤙ l
        layerOpacity    ← arr (fromMaybe 1 . listToMaybe)
                          . listA (getAttrR "opacity")     ⤙ l
        layerIsVisible  ← arr (isNothing . listToMaybe)
                          . listA (getAttrValue "visible") ⤙ l
        layerProperties ← properties                      ⤙ l
        returnA ⤙ case x of Left  layerData    → Layer {..}
                            Right layerObjects → ObjectLayer {..}

tilesets ∷ FilePath → IOSArrow XmlTree [Tileset]
tilesets mapPath = listA $ getChildren >>> isElem >>> hasName "tileset"
                   >>> getAttrR "firstgid"
                   &&& when (hasAttr "source") (externalTileset mapPath)
                   >>> tileset

externalTileset ∷ FilePath → IOSArrow XmlTree XmlTree
externalTileset mapPath = arr (const (dropFileName mapPath)) &&& getAttrValue "source"
                          >>> arr (uncurry (</>))
                          >>> readFromDocument [ withValidate no, withWarnings yes ]
                          >>> getChildren >>> isElem >>> hasName "tileset"

tileset ∷ IOSArrow (Word32, XmlTree) Tileset
tileset = proc (firstGid, ts) → do
  tsName           ← getAttrValue "name"                        ⤙ ts
  tsInitialGid     ← id                                         ⤙ firstGid
  tsTileWidth      ← getAttrR "tilewidth"                       ⤙ ts
  tsTileHeight     ← getAttrR "tileheight"                      ⤙ ts
  tsMargin         ← arr (fromMaybe 0) . getAttrMaybe "margin"  ⤙ ts
  tsSpacing        ← arr (fromMaybe 0) . getAttrMaybe "spacing" ⤙ ts
  tsImages         ← images                                     ⤙ ts
  tsTileProperties ← listA tileProperties                       ⤙ ts
  returnA ⤙ Tileset {..}
  where tileProperties ∷ IOSArrow XmlTree (Word32, Properties)
        tileProperties = getChildren >>> isElem >>> hasName "tile"
                         >>> getAttrR "id" &&& properties
        images = listA (getChildren >>> image)

image ∷ IOSArrow XmlTree Image
image = isElem >>> hasName "image" >>> proc img → do
    iSource ← getAttrValue "source"   ⤙ img
    iTrans  ← arr (fmap colorToTriplet . listToMaybe) . listA (getAttrValue0 "trans") ⤙ img
    iWidth  ← getAttrR "width"        ⤙ img
    iHeight ← getAttrR "height"       ⤙ img
    returnA ⤙ Image {..}
    where
        colorToTriplet x = (h x, h $ drop 2 x, h $ drop 4 x)
            where h (y:z:_) = fromIntegral $ digitToInt y * 16 + digitToInt z
                  h _ = error "invalid color in an <image ...> somewhere."
