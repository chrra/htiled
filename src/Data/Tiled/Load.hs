{-# LANGUAGE Arrows          #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
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
import           Data.Tree.NTree.TypeDefs   (NTree)
import           Data.Word                  (Word32)
import           Prelude                    hiding (id, (.))

import qualified Codec.Compression.GZip     as GZip
import qualified Codec.Compression.Zlib     as Zlib
import           System.FilePath            (dropFileName, (</>))
import           Text.XML.HXT.Core

import           Data.Tiled.Types

-- | Load a map from a string
loadMap :: String -> IO TiledMap
loadMap str = load (readString [] str) "binary"

-- | Load a map file.
loadMapFile :: FilePath -> IO TiledMap
loadMapFile fp = load (readDocument [] fp) fp

load :: IOStateArrow () XmlTree XmlTree -> FilePath -> IO TiledMap
load a fp = head `fmap` runX (
        configSysVars [withValidate no, withWarnings yes]
    >>> a
    >>> getChildren >>> isElem
    >>> doMap fp)

getAttrR :: (Read a, Num a) => String -> IOSArrow XmlTree a
getAttrR a = arr read . getAttrValue0 a

getAttrMaybeR :: (Read a, Num a) => String -> IOSArrow XmlTree (Maybe a)
getAttrMaybeR a = arr (fmap read) . getAttrMaybe a

getAttrMaybe :: String -> IOSArrow XmlTree (Maybe String)
getAttrMaybe a = arr tm . getAttrValue a
    where
        tm "" = Nothing
        tm s  = Just s

properties :: IOSArrow XmlTree Properties
properties = listA $ getChildren >>> isElem >>> hasName "properties"
            >>> getChildren >>> isElem >>> hasName "property"
            >>> getAttrValue "name" &&& getAttrValue "value"

frame :: IOSArrow XmlTree Frame
frame = getChildren >>> isElem >>> hasName "frame" >>> proc xml -> do
  frameTileId   <- getAttrR "tileid"   -< xml
  frameDuration <- getAttrR "duration" -< xml
  returnA -< Frame{..}

animation :: IOSArrow XmlTree Animation
animation = getChildren >>> isElem >>> hasName "animation"
                        >>> listA frame
                        >>> arr Animation

tile :: IOSArrow XmlTree Tile
tile = getChildren >>> isElem >>> hasName "tile" >>> getTile
 where getTile :: IOSArrow XmlTree Tile
       getTile = proc xml -> do
         tileId          <- getAttrR "id"                 -< xml
         tileProperties  <- properties                    -< xml
         tileImage       <- arr listToMaybe . listA image -< xml
         --tileObjectGroup <- arr listToMaybe . listA doObjectGroup -< xml
         tileAnimation   <- arr listToMaybe . listA animation -< xml
         returnA -< Tile{..}

doMap :: FilePath -> IOSArrow XmlTree TiledMap
doMap mapPath = proc m -> do
    mapOrientation <- arr (\case "orthogonal" -> Orthogonal
                                 "isometric"  -> Isometric
                                 _            -> error "unsupported orientation")
                     . getAttrValue "orientation" -< m
    mapWidth       <- getAttrR "width"      -< m
    mapHeight      <- getAttrR "height"     -< m
    mapTileWidth   <- getAttrR "tilewidth"  -< m
    mapTileHeight  <- getAttrR "tileheight" -< m
    mapProperties  <- properties            -< m
    mapTilesets    <- tilesets mapPath      -< m
    mapLayers      <- layers                -< (m, (mapWidth, mapHeight))
    returnA        -< TiledMap {..}
--------------------------------------------------------------------------------
-- Layer
--------------------------------------------------------------------------------
-- | When you use the tile flipping feature added in Tiled Qt 0.7, the highest
-- two bits of the gid store the flipped state. Bit 32 is used for storing
-- whether the tile is horizontally flipped and bit 31 is used for the
-- vertically flipped tiles. And since Tiled Qt 0.8, bit 30 means whether the
-- tile is flipped (anti) diagonally, enabling tile rotation. These bits have to be read and cleared before you can find out which tileset a tile belongs to.
-- When rendering a tile, the order of operation matters. The diagonal flip
-- (x/y axis swap) is done first, followed by the horizontal and vertical flips.
wordsToIndices :: [Word32] -> [TileIndex]
wordsToIndices []           = []
wordsToIndices (w:ws)       = TileIndex { .. } : wordsToIndices ws
  where tileIndexGid           = w `clearBit` 30 `clearBit` 31 `clearBit` 29
        tileIndexIsVFlipped    = w `testBit` 30
        tileIndexIsHFlipped    = w `testBit` 31
        tileIndexIsDiagFlipped = w `testBit` 29

points :: String -> [(Int, Int)]
points [] = []
points s  = (px, py):points rest
    where (p, rest) = drop 1 `fmap` break (==' ') s
          (x', y') = drop 1 `fmap` break (==',') p
          px = read x'
          py = read y'

polygon :: IOSArrow XmlTree [Polygon]
polygon = listA $ getChildren >>> isElem
                              >>> hasName "polygon"
                              >>> getAttrValue "points"
                              >>> arr (Polygon . points)

polyline :: IOSArrow XmlTree [Polyline]
polyline = listA $ getChildren >>> isElem
                               >>> hasName "polyline"
                               >>> getAttrValue "points"
                               >>> arr (Polyline . points)

object :: IOSLA (XIOState ()) (NTree XNode) Object
object = getChildren >>> isElem >>> hasName "object" >>> proc obj -> do
  objectName       <- arr listToMaybe . listA (getAttrValue "name") -< obj
  objectType       <- arr listToMaybe . listA (getAttrValue "type") -< obj
  objectX          <- getAttrR "x"                                  -< obj
  objectY          <- getAttrR "y"                                  -< obj
  objectWidth      <- arr listToMaybe . listA (getAttrR "width")    -< obj
  objectHeight     <- arr listToMaybe . listA (getAttrR "height")   -< obj
  objectGid        <- arr listToMaybe . listA (getAttrR "gid")      -< obj
  objectPolygon    <- arr listToMaybe . polygon                     -< obj
  objectPolyline   <- arr listToMaybe . polyline                    -< obj
  objectProperties <- properties                                  -< obj
  returnA      -< Object {..}

doObjectGroup :: IOSLA (XIOState ()) (XmlTree, b) Layer
doObjectGroup = arr fst >>> hasName "objectgroup"
                        >>> id &&& (listA object >>> arr Right)
                        >>> common

doLayerProps :: IOSLA (XIOState ()) XmlTree (String, Float, Bool, Properties)
doLayerProps = proc l -> do
    name       <- getAttrValue "name" -< l
    opacity    <- arr (fromMaybe (1 :: Float) . listToMaybe)
                    . listA (getAttrR "opacity") -< l
    visibility <- arr (isNothing . listToMaybe)
                    . listA (getAttrValue "visible") -< l
    props      <- properties -< l
    returnA -< (name, opacity, visibility, props)

doImageLayer :: IOSLA (XIOState ()) (XmlTree, b) Layer
doImageLayer = arr fst >>> hasName "imagelayer"
                       >>> id &&& image
                       >>> proc (l, layerImage) -> do
  (layerName, layerOpacity, layerIsVisible, layerProperties) <-
    doLayerProps -< l
  returnA -< ImageLayer{..}

doLayer :: IOSLA (XIOState ()) (XmlTree, (Int, Int)) Layer
doLayer = first (hasName "layer") >>> arr fst &&& (doData >>> arr Left) >>> common

doData :: IOSLA (XIOState ()) (NTree XNode, (Int, Int)) (Map (Int, Int) TileIndex)
doData = first (getChildren >>> isElem >>> hasName "data")
      >>> proc (dat, (w, h)) -> do
            encoding    <- getAttrValue "encoding"        -< dat
            compression <- getAttrValue "compression"     -< dat
            text        <- getText . isText . getChildren -< dat
            returnA -< dataToIndices w h encoding compression text

-- Width -> Height -> Encoding -> Compression -> Data -> [Tile]
dataToIndices :: Int -> Int -> String -> String -> String -> Map (Int, Int) TileIndex
dataToIndices w h "base64" "gzip" = toMap w h . base64 GZip.decompress
dataToIndices w h "base64" "zlib" = toMap w h . base64 Zlib.decompress
dataToIndices w h "csv"    _      = toMap w h . csv
dataToIndices _ _ _ _ = error "unsupported tile data format, only base64 with \
                            \gzip/zlib and csv are supported at the moment."

toMap :: Int -> t -> [TileIndex] -> Map (Int, Int) TileIndex
toMap w _ = fromDistinctAscList . sort . filter (\(_, x) -> tileIndexGid x /= 0)
            . zipWith (\ndx t -> ((mkX w ndx, mkY w ndx), t)) [0..]

mkX,mkY :: Int -> Int -> Int
mkX w ndx = ndx - mkY w ndx * fromIntegral w
mkY w ndx = floor $ (fromIntegral ndx :: Double) / fromIntegral w

base64 :: (LBS.ByteString -> LBS.ByteString) -> String -> [TileIndex]
base64 f = wordsToIndices . bytesToWords . LBS.unpack . f . LBS.fromChunks
                        . (:[]) . B64.decodeLenient . BS.pack

csv :: String -> [TileIndex]
csv = wordsToIndices . map (read :: String -> Word32)
                   . splitOn ","
                   . filter (`elem` (',':['0' .. '9']))

bytesToWords :: [Char] -> [Word32]
bytesToWords []           = []
bytesToWords (a:b:c:d:xs) = n : bytesToWords xs
  where n = f a + f b * 256 + f c * 65536 + f d * 16777216
        f = fromIntegral . fromEnum :: Char -> Word32
bytesToWords _            = error "number of bytes not a multiple of 4."

common
  :: IOSLA (XIOState ()) (XmlTree, Either (Map (Int, Int) TileIndex) [Object]) Layer
common = proc (l, px) -> do
    (layerName, layerOpacity, layerIsVisible, layerProperties) <-
      doLayerProps -< l
    returnA -< case px of Left  layerData    -> Layer {..}
                          Right layerObjects -> ObjectLayer {..}

layers :: IOSArrow (XmlTree, (Int, Int)) [Layer]
layers = listA (first (getChildren >>> isElem) >>> doObjectGroup
                                               <+> doLayer
                                               <+> doImageLayer)
--------------------------------------------------------------------------------
-- TileSet
--------------------------------------------------------------------------------
tilesets :: FilePath -> IOSArrow XmlTree [Tileset]
tilesets fp =
  listA $ getChildren >>> isElem >>> hasName "tileset"
  >>> getAttrR "firstgid" &&& ifA (hasAttr "source") (externalTileset fp) id
  >>> tileset

externalTileset :: FilePath -> IOSArrow XmlTree XmlTree
externalTileset mapPath =
  arr (const (dropFileName mapPath)) &&& getAttrValue "source"
  >>> arr (uncurry (</>))
  >>> readFromDocument [ withValidate no, withWarnings yes ]
  >>> getChildren >>> isElem >>> hasName "tileset"

tileset :: IOSArrow (Word32, XmlTree) Tileset
tileset = proc (tsInitialGid, ts) -> do
  tsName           <- getAttrValue "name"                           -< ts
  tsTileWidth      <- getAttrR "tilewidth"                          -< ts
  tsTileHeight     <- getAttrR "tileheight"                         -< ts
  tsTileCount      <- arr (fromMaybe 0) . getAttrMaybeR "tilecount" -< ts
  tsMargin         <- arr (fromMaybe 0) . getAttrMaybeR "margin"    -< ts
  tsSpacing        <- arr (fromMaybe 0) . getAttrMaybeR "spacing"   -< ts
  tsImages         <- images                                     -< ts
  --tsTileProperties <- listA tileProperties                          -< ts
  tsProperties <- listA properties -< ts
  tsTiles      <- listA tile       -< ts
  returnA -< Tileset {..}
    where images = listA (getChildren >>> image)

data ImageType = TileImage | NormalImage deriving (Read)

image :: IOSArrow XmlTree Image
image = isElem >>> hasName "image" >>> proc img -> do
    iWidth  <- getAttrR "width"        -< img
    iHeight <- getAttrR "height"       -< img
    iSource <- getAttrValue0 "source"  -< img
    iTrans  <- arr (fmap colorToTriplet) . getAttrMaybe "trans" -< img
    returnA -< Image {..}
    where
        colorToTriplet :: Integral i => String -> (i,i,i)
        colorToTriplet x = (h x , h (drop 2 x) , h (drop 4 x))
            where h (y:z:_) = fromIntegral $ digitToInt y * 16 + digitToInt z
                  h _ = error "invalid color in an <image ...> somewhere."

--tileImage :: IOSArrow XmlTree Image
--tileImage = isElem >>> hasName "tile" >>> getChildren >>> image
