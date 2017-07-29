{-# LANGUAGE Arrows          #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Data.Tiled.Load
  (loadMapFile
  , loadMap
  , properties
  , tile
  , tileset
  , doLayer
  , doMap
  , load
  , loadApply
  ) where

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import           Control.Category ((.))
import           Data.Bits (clearBit, testBit)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (digitToInt)
import           Data.List.Split (splitOn)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Tree.NTree.TypeDefs (NTree)
import           Data.Vector (fromList, unfoldr)
import           Data.Word (Word32)
import           Prelude hiding (id, (.))
import           System.FilePath (dropFileName, (</>))
import           Text.XML.HXT.Core

import           Data.Tiled.Types

-- | Load a map from a string
loadMap :: String -> IO TiledMap
loadMap str = load (readString [] str) "binary"

-- | Load a map file.
loadMapFile :: FilePath -> IO TiledMap
loadMapFile fp = load (readDocument [] fp) fp

loadApply :: IOStateArrow () XmlTree XmlTree
          -> IOStateArrow () XmlTree a
          -> IO [a]
loadApply generateXmlA readDataA =
  runX (
        configSysVars [withValidate no, withWarnings yes]
    >>> generateXmlA
    >>> getChildren >>> isElem
    >>> readDataA)

load :: IOSArrow XmlTree XmlTree -> FilePath -> IO TiledMap
load a fp = head <$> loadApply a (doMap fp)

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
properties =
  hasName "properties" >>> listA
  (getChildren >>> property)
  where
    property =
      hasName "property" >>>
      proc xml -> do
          propName <- getAttrValue "name" -< xml
          propValue <- getAttrValue "value" -< xml
          returnA -< (propName,propValue)

entityProperties :: IOSArrow XmlTree Properties
entityProperties =
  arr (fromMaybe [] . listToMaybe) <<< listA (properties <<< getChildren)

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
tile = isElem >>> hasName "tile" >>> getTile
 where
   getTile :: IOSArrow XmlTree Tile
   getTile = proc xml -> do
     tileId          <- getAttrR "id" -< xml
     tileProperties <- entityProperties -< xml
     tileImage       <- arr listToMaybe .
                        listA (image <<< getChildren) -< xml
     tileObjectGroup <- flip withDefault [] doObjectGroup -< xml
     tileAnimation   <- arr listToMaybe . listA animation -< xml
     returnA -< Tile{..}

doMap :: FilePath -> IOSArrow XmlTree TiledMap
doMap mapPath = proc m -> do
    mapOrientation <- arr parseOrientation
                     . getAttrValue "orientation" -< m
    mapWidth       <- getAttrR "width"      -< m
    mapHeight      <- getAttrR "height"     -< m
    mapTileWidth   <- getAttrR "tilewidth"  -< m
    mapTileHeight  <- getAttrR "tileheight" -< m
    mapProperties  <- entityProperties -< m
    mapTilesets    <- tilesets mapPath      -< m
    mapLayers      <- layers -< (m, (mapWidth, mapHeight))
    returnA        -< TiledMap {..}
  where
    parseOrientation = \case
      "orthogonal" -> Orthogonal
      "isometric"  -> Isometric
      _            -> error "unsupported orientation"

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

doObjectGroup :: IOSLA (XIOState ()) XmlTree [Object]
doObjectGroup = hasName "objectgroup" >>> listA object

dataToIndices :: Int -> String -> String -> String -> TileData
dataToIndices w "base64" "gzip" = toVector w . base64 GZip.decompress
dataToIndices w "base64" "zlib" = toVector w . base64 Zlib.decompress
dataToIndices w "csv"    _      = toVector w . csv
dataToIndices _ _ _ = error "unsupported tile data format, only base64 with \
                          \gzip/zlib and csv are supported at the moment."

toVector :: Int -> [TileIndex] -> TileData
toVector w = unfoldr (f . splitAt w)
   where f ([], _)   = Nothing
         f (x, rest) = Just (fromList $ map tileToMaybe x, rest)
         tileToMaybe t@TileIndex {..} | tileIndexGid == 0 = Nothing
                                      | otherwise = Just t

base64 :: (LBS.ByteString -> LBS.ByteString) -> String -> [TileIndex]
base64 f = wordsToIndices . bytesToWords . LBS.unpack . f . LBS.fromChunks
                        . (:[]) . B64.decodeLenient . BS.pack

csv :: String -> [TileIndex]
csv = wordsToIndices . map (read :: String -> Word32)
                   . splitOn ","
                   . filter (`elem` (',':['0' .. '9']))

bytesToWords :: String -> [Word32]
bytesToWords []           = []
bytesToWords (a:b:c:d:xs) = n : bytesToWords xs
  where n = f a + f b * 256 + f c * 65536 + f d * 16777216
        f = fromIntegral . fromEnum :: Char -> Word32
bytesToWords _            = error "number of bytes not a multiple of 4."

layers :: IOSArrow (XmlTree, (Int, Int)) [Layer]
layers = listA (first getChildren >>> doLayer)

doLayer :: IOSArrow (XmlTree, (Int,Int)) Layer
doLayer = proc (xml,(w,_)) -> do
  layerData <- doLayerData -< (xml,w)
  l <- doLayerCommon -< (xml,layerData)
  returnA -< l

doLayerData :: IOSArrow (XmlTree, Int) LayerContents
doLayerData =
  (doData >>> arr LayerContentsTiles) <+>
  ( arr fst >>>
    hasName "imagelayer" >>>
    getChildren >>>
    image >>>
    arr LayerContentsImage) <+>
  (arr fst >>> doObjectGroup >>> arr LayerContentsObjects)

doData :: IOSArrow (XmlTree, Int) TileData
doData =
  first (hasName "layer" >>>
         getChildren >>>
         isElem >>>
         hasName "data") >>>
  proc (dat, w) -> do
    encoding    <- getAttrValue "encoding"        -< dat
    compression <- getAttrValue "compression"     -< dat
    text        <- getText . isText . getChildren -< dat
    returnA -< dataToIndices w encoding compression text

doLayerCommon :: IOSArrow (XmlTree, LayerContents) Layer
doLayerCommon = proc (l, layerContents) -> do
  layerName       <- getAttrValue "name" -< l
  layerOpacity    <- arr (fromMaybe (1 :: Float) . listToMaybe)
                . listA (getAttrR "opacity") -< l
  layerIsVisible <- arr (convertVisibility)
                    . getAttrMaybeR "visible" -< l
  layerProperties <- entityProperties -< l
  offsetx    <- arr (fromMaybe (0 :: Int) . listToMaybe)
                . listA (getAttrR "offsetx") -< l
  offsety    <- arr (fromMaybe (0 :: Int) . listToMaybe)
                . listA (getAttrR "offsety") -< l
  let
    layerOffset = (offsetx,offsety)
  returnA -< Layer{..}
  where
    convertVisibility :: Maybe Int -> Bool
    convertVisibility Nothing = True
    convertVisibility (Just 1) = True
    convertVisibility (Just 0) = False
    convertVisibility _ = error "visibility other than 1 and 0 is not supported"

tilesets :: FilePath -> IOSArrow XmlTree [Tileset]
tilesets fp = listA (getChildren >>> tileset fp)

tileset :: FilePath -> IOSArrow XmlTree Tileset
tileset fp =
  (arr (Nothing,) >>> internalTileset) <+>
  externalTileset fp

externalTileset :: FilePath -> IOSArrow XmlTree Tileset
externalTileset mapPath = hasName "tileset" >>>
  hasAttr "source" >>> proc xml -> do
    source <- arr ((dropFileName mapPath) </>) <<<
              getAttrValue "source" -< xml
    gid <- getAttrR "firstgid" -< xml
    externalDocument <-
      readFromDocument [ withValidate no, withWarnings yes] -< source
    ts <- internalTileset  <<<
          second getChildren <<<
          first (arr Just) -< (gid,externalDocument)
    returnA -< ts

internalTileset :: IOSArrow (Maybe Word32, XmlTree) Tileset
internalTileset = second (isElem >>> hasName "tileset") >>>
  proc (maybeGid,ts) -> do
    tsName <- getAttrValue "name" -< ts
    maybeGidHere <- getAttrMaybeR "firstgid" -< ts
    tsTileWidth <- getAttrR "tilewidth" -< ts
    tsTileHeight <- getAttrR "tileheight" -< ts
    tsTileCount <- arr (fromMaybe 0) .
                   getAttrMaybeR "tilecount" -< ts
    tsMargin <- arr (fromMaybe 0) . getAttrMaybeR "margin" -< ts
    tsSpacing <- arr (fromMaybe 0) . getAttrMaybeR "spacing" -< ts
    tsImages <- images -< ts
    tsColumns <- getAttrR "columns" -< ts
    tsProperties <- entityProperties -< ts
    tsTiles <- listA (tile <<< getChildren) -< ts
    let
      tsInitialGid =
        fromMaybe (error "Cannot load gid from tileset") $
        maybe maybeGidHere Just $
        maybeGid
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
