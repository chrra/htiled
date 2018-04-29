{-# LANGUAGE RecordWildCards #-}
module Data.Tiled.Types where

import           Data.Vector (Vector)
import           Data.Word   (Word32, Word8)

-- | Orientations.
data MapOrientation = Orthogonal | Isometric deriving (Show, Eq)

-- | Properties.
type Properties = [(String, String)]

-- | A tiled map.
data TiledMap = TiledMap
         { mapPath             :: FilePath -- ^ The file path of the map file.
         , mapOrientation      :: MapOrientation
         , mapWidth, mapHeight :: Int
         , mapTileWidth        :: Int
         , mapTileHeight       :: Int
         , mapProperties       :: Properties
         , mapTilesets         :: [Tileset]
         , mapLayers           :: [Layer]
         } deriving (Show, Eq)

-- | A set of tiles that can be used.
data Tileset = Tileset
             { tsName                    :: String
             , tsInitialGid              :: Word32
             , tsTileWidth, tsTileHeight :: Int
             , tsTileCount               :: Int
             , tsSpacing, tsMargin       :: Int
             , tsImages                  :: [Image] -- ^ Multiple images not
                                                    -- yet supported in tiled.
             , tsProperties              :: Properties
             , tsTiles                   :: [Tile]
             , tsColumns                 :: Int
             } deriving (Show, Eq)

-- | One frame of an animation.
data Frame = Frame { frameTileId   :: Word32
                   -- ^ The local ID of a tile within the parent TileSet.
                   , frameDuration :: Int
                   -- ^ How long (in milliseconds) this frame should be
                   -- displayed before advancing to the next frame.
                   } deriving (Show, Eq, Ord)

-- | Contains a list of animation frames.
newtype Animation = Animation { animationFrames :: [Frame] }
                  deriving (Show, Eq, Ord)

-- | A tile as defined in a TileSet.
data Tile = Tile { tileId          :: Word32
                 -- ^ The local tile ID within its tileset.
                 -- TODO: Add terrain and probability
                 , tileProperties  :: Properties
                 , tileImage       :: Maybe Image
                 , tileObjectGroup :: [Object]
                 , tileAnimation   :: Maybe Animation
                 } deriving (Show, Eq)

-- | An image containing tiles.
data Image = Image
           { iSource         :: FilePath
           , iTrans          :: Maybe (Word8, Word8, Word8)
           , iWidth, iHeight :: Int
           } deriving (Show, Eq)

-- | An object, usable for stuff not repetitively aligned on a grid.
data Object = Object
            { objectName       :: Maybe String
            , objectType       :: Maybe String
            , objectIsVisible  :: Bool
            , objectProperties :: Properties
            , objectX, objectY :: Double
            , objectKind       :: ObjectKind
            } deriving (Show, Eq)

-- | Object rotation.
type Rotation = Double

-- | Object width.
type Width = Double

-- | Object height.
type Height = Double

-- | The different types of objects that can be placed.
data ObjectKind = ObjectKindPoint
                | ObjectKindRectangle Width Height Rotation
                | ObjectKindEllipse Width Height Rotation
                | ObjectKindPolygon Polygon Rotation
                | ObjectKindPolyline Polyline Rotation
                | ObjectKindTile Width Height Rotation TileIndex
                | ObjectKindText Width Height Rotation TextData
                deriving (Show, Eq)

-- | Horizontal alignment within a text object.
data TextHorizontalAlignment = TextHAlignmentLeft
                             | TextHAlignmentCenter
                             | TextHAlignmentRight
                             deriving (Show, Eq)

-- | Vertical alignment within a text object.
data TextVerticalAlignment = TextVAlignmentTop
                           | TextVAlignmentCenter
                           | TextVAlignmentBottom
                           deriving (Show, Eq)

-- | Text object specific data.
data TextData = TextData 
              { textContents            :: String
              , textFontFamily          :: String
              , textPixelSize           :: Int
              , textWordWrap            :: Bool
              , textColor               :: (Word8, Word8, Word8, Word8)
              -- ^ Text color in ARGB.
              , textHorizontalAlignment :: TextHorizontalAlignment
              , textVerticalAlignment   :: TextVerticalAlignment
              , textBold                :: Bool
              , textItalic              :: Bool
              , textUnderline           :: Bool
              , textStrikeout           :: Bool
              } deriving (Show, Eq)

-- | A polygon.
newtype Polygon = Polygon [(Int, Int)] deriving (Show, Eq)

-- | A polyline.
newtype Polyline = Polyline [(Int, Int)] deriving (Show, Eq)

-- | A single tile index as is stored in a layer.
data TileIndex = TileIndex { tileIndexGid           :: Word32
                           , tileIndexIsVFlipped    :: Bool
                           , tileIndexIsHFlipped    :: Bool
                           , tileIndexIsDiagFlipped :: Bool
                           } deriving (Show, Eq, Ord)

-- | A collection of multidimensional tile data.
type TileData = Vector (Vector (Maybe TileIndex))

-- | A layer's tile contents.
data LayerContents = LayerContentsTiles   TileData
                   | LayerContentsObjects [Object]
                   | LayerContentsImage   Image
                   deriving (Eq)

instance Show LayerContents where
    show (LayerContentsTiles _)      = "LayerContentsTiles ..."
    show (LayerContentsObjects objs) = "LayerContentsObjects " ++ show objs
    show (LayerContentsImage img)    = "LayerContentsImage "   ++ show img

-- | An entire layer of tiled data.
data Layer = Layer { layerName       :: String
                   , layerOpacity    :: Float
                   , layerIsVisible  :: Bool
                   , layerProperties :: Properties
                   , layerOffset     :: (Int, Int)
                   , layerContents   :: LayerContents
                   } deriving (Show, Eq)
