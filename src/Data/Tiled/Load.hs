{-# LANGUAGE Arrows, UnicodeSyntax, RecordWildCards #-}
module Data.Tiled.Load (loadMapFile, loadMap) where

import Prelude hiding ((.), id)
import Control.Category ((.), id)
import Data.Bits (testBit, clearBit)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (digitToInt)
import Data.List (unfoldr)
import Data.Vector (fromList, Vector)
import Data.Maybe (listToMaybe, fromMaybe, isNothing)
import Data.Word (Word32)

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import Text.XML.HXT.Core

import Data.Tiled.Types

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
    mapTilesets    ← tilesets              ⤙ m
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
    dataToTiles ∷ Int → Int → String → String → String
                → Vector (Vector (Maybe Tile))
    dataToTiles w h "base64" "gzip" = toVector w h . base64 GZip.decompress
    dataToTiles w h "base64" "zlib" = toVector w h . base64 Zlib.decompress
    dataToTiles _ _ _ _ = error "unsupported tile data format, only base64 and \
                                \gzip/zlib is supported at the moment."

    toVector w _ = fromList . unfoldr (f . splitAt w)
      where f ([], _) = Nothing
            f (x, rest) = Just (fromList $ map tileToMaybe x, rest)
            tileToMaybe t@Tile {..} | tileGid == 0 = Nothing
                                    | otherwise = Just t

    base64 f = bytesToTiles . LBS.unpack . f . LBS.fromChunks
                            . (:[]) . B64.decodeLenient . BS.pack

    bytesToTiles (a:b:c:d:xs) = Tile { .. } : bytesToTiles xs
      where n = f a + f b * 256 + f c * 65536 + f d * 16777216
            f = fromIntegral . fromEnum ∷ Char → Word32
            tileGid = n `clearBit` 30 `clearBit` 31 `clearBit` 29
            tileIsVFlipped = n `testBit` 30
            tileIsHFlipped = n `testBit` 31
            tileIsDiagFlipped = n `testBit` 29
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
              tsMargin      ← (arr $ fromMaybe 0) . getAttrMaybe "margin" ⤙ ts
              tsSpacing     ← (arr $ fromMaybe 0) . getAttrMaybe "spacing" ⤙ ts
              tsImages      ← images                  ⤙ ts
              tsTileProperties ← listA tileProperties ⤙ ts
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
