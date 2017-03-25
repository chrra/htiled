{-# LANGUAGE RecordWildCards #-}
module Data.Tiled.Types where

import           Data.Map  (Map)
import           Data.Word (Word32, Word8)

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
             , tsSpacing, tsMargin       :: Int
             , tsImages                  :: [Image] -- ^ Multiple images not
                                                   -- yet supported in tiled.
             , tsTileProperties          :: [(Word32, Properties)]
             } deriving (Show, Eq)

-- | An image containing tiles.
data Image = Image
           { iSource         :: FilePath
           , iTrans          :: Maybe (Word8, Word8, Word8)
           , iWidth, iHeight :: Int
           } deriving (Show, Eq)

-- | An object, usable for stuff not repetitively aligned on a grid.
data Object = Object
            { objectName                :: Maybe String
            , objectType                :: Maybe String
            , objectProperties          :: Properties
            , objectX, objectY          :: Int
            , objectWidth, objectHeight :: Maybe Int
            , objectGid                 :: Maybe Word32
            , objectPolygon             :: Maybe Polygon
            , objectPolyline            :: Maybe Polyline
            } deriving (Show, Eq)

-- | A polygon.
newtype Polygon = Polygon [(Int, Int)] deriving (Show, Eq)

-- | A polyline.
newtype Polyline = Polyline [(Int, Int)] deriving (Show, Eq)

-- | Either a tile layer or an object layer.
data Layer = Layer
           { layerName       :: String
           , layerOpacity    :: Float
           , layerIsVisible  :: Bool
           , layerProperties :: Properties
           , layerData       :: Map (Int, Int) Tile
           }
           | ObjectLayer
           { layerName       :: String
           , layerOpacity    :: Float
           , layerIsVisible  :: Bool
           , layerProperties :: Properties
           , layerObjects    :: [Object]
           }
           | ImageLayer
           { layerName       :: String
           , layerOpacity    :: Float
           , layerIsVisible  :: Bool
           , layerProperties :: Properties
           , layerImage      :: Image
           } deriving Eq

-- | One frame of an animation.
data Frame = Frame { frameTileId   :: Int
                   -- ^ The local ID of a tile within the parent TileSet.
                   , frameDuration :: Int
                   -- ^ How long (in milliseconds) this frame should be
                   -- displayed before advancing to the next frame.
                   } deriving (Show, Eq, Ord)

-- | Contains a list of animation frames.
newtype Animation = Animation { animationFrames :: [Frame] }
                  deriving (Show, Eq, Ord)

-- | A single tile as is stored in a layer.
data Tile = Tile { tileGid                                           :: Word32
                 , tileIsVFlipped, tileIsHFlipped, tileIsDiagFlipped :: Bool
                 , tileAnimation :: Maybe Animation
                 } deriving (Show, Eq, Ord)


instance Show Layer where
    show Layer {..} = "Layer { layerName = " ++ show layerName ++
                            ", layerOpacity = " ++ show layerOpacity ++
                            ", layerIsVisible = " ++ show layerIsVisible ++
                            ", layerProperties = " ++ show layerProperties ++
                            ", layerData = \"...\" }"
    show ObjectLayer {..} = "ObjectLayer { layerName = " ++ show layerName ++
                                        ", layerOpacity = " ++ show layerOpacity ++
                                        ", layerIsVisible = " ++ show layerIsVisible ++
                                        ", layerProperties = " ++ show layerProperties ++
                                        ", layerObjects = " ++ show layerObjects ++ " }"
    show ImageLayer {..} = "ObjectLayer { layerName = " ++ show layerName ++
                                       ", layerOpacity = " ++ show layerOpacity ++
                                       ", layerIsVisible = " ++ show layerIsVisible ++
                                       ", layerProperties = " ++ show layerProperties ++
                                       ", layerImage = " ++ show layerImage ++ " }"
