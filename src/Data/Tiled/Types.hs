{-# LANGUAGE UnicodeSyntax, RecordWildCards #-}
module Data.Tiled.Types where

import Data.Vector (Vector)
import Data.Word (Word8, Word32)

-- | Orientations.
data MapOrientation = Orthogonal | Isometric deriving (Show, Eq)

-- | Properties.
type Properties = [(String, String)]

-- | A tiled map.
data TiledMap = TiledMap
         { mapPath             ∷ FilePath -- ^ The file path of the map file.
         , mapOrientation      ∷ MapOrientation
         , mapWidth, mapHeight ∷ Int
         , mapTileWidth        ∷ Int
         , mapTileHeight       ∷ Int
         , mapProperties       ∷ Properties
         , mapTilesets         ∷ [Tileset]
         , mapLayers           ∷ [Layer]
         } deriving (Show, Eq)

-- | A set of tiles that can be used.
data Tileset = Tileset
             { tsName                    ∷ String
             , tsInitialGid              ∷ Word32
             , tsTileWidth, tsTileHeight ∷ Int
             , tsSpacing, tsMargin       ∷ Int
             , tsImages                  ∷ [Image] -- ^ Multiple images not
                                                   -- yet supported in tiled.
             , tsTileProperties          ∷ [(Word32, Properties)]
             } deriving (Show, Eq)

-- | An image containing tiles.
data Image = Image
           { iSource         ∷ FilePath
           , iTrans          ∷ Maybe (Word8, Word8, Word8)
           , iWidth, iHeight ∷ Int
           } deriving (Show, Eq)

-- | An object, usable for stuff not repetitively aligned on a grid.
data Object = Object
            { objectName                ∷ Maybe String
            , objectType                ∷ Maybe String
            , objectProperties          ∷ Properties
            , objectX, objectY          ∷ Int
            , objectWidth, objectHeight ∷ Maybe Int
            , objectGid                 ∷ Maybe Word32
            , objectPolygon             ∷ Maybe Polygon
            , objectPolyline            ∷ Maybe Polyline
            } deriving (Show, Eq)

-- | A polygon.
data Polygon = Polygon [(Int, Int)] deriving (Show, Eq)

-- | A polyline.
data Polyline = Polyline [(Int, Int)] deriving (Show, Eq)

-- | Either a tile layer or an object layer.
data Layer = Layer
           { layerName       ∷ String
           , layerOpacity    ∷ Float
           , layerIsVisible  ∷ Bool
           , layerProperties ∷ Properties
           , layerData       ∷ Vector (Vector (Maybe Tile))
           }
           | ObjectLayer
           { layerName       ∷ String
           , layerOpacity    ∷ Float
           , layerIsVisible  ∷ Bool
           , layerProperties ∷ Properties
           , layerObjects    ∷ [Object]
           }
           | ImageLayer
           { layerName       ∷ String
           , layerOpacity    ∷ Float
           , layerIsVisible  ∷ Bool
           , layerProperties ∷ Properties
           , layerImage      ∷ Image
           } deriving Eq

-- | A single tile as is stored in a layer.
data Tile = Tile { tileGid                        ∷ Word32
                 , tileIsVFlipped, tileIsHFlipped, tileIsDiagFlipped ∷ Bool
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

