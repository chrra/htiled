{-# LANGUAGE OverloadedStrings #-}

module Data.Tiled.Test.Xml where

import           Data.ByteString.Lazy.Char8 as BS
import           Data.String
import qualified Prelude
import           Prelude hiding (show)
import           Text.XML.Generator
import           Text.XML.HXT.Core

import           Data.Tiled.Load
import           Data.Tiled.Types

show :: (Show a, IsString s) => a -> s
show = fromString . Prelude.show

parseXml action xmlElems = Prelude.head <$>
  loadApply (readString [] (BS.unpack $ xrender xmlElems)) action

tilesetToXml tileset =
  xelem "tileset" (tilesetAttrs,tilesetChildren)
  where
    tilesetAttrs = xattrs
      [ xattr "name" (fromString $ tsName tileset)
      , xattr "firstgid" (show . tsInitialGid $ tileset)
      , xattr "columns" (show . tsColumns $ tileset)
      , xattr "tilewidth" (show . tsTileWidth $ tileset)
      , xattr "tileheight" (show . tsTileHeight $ tileset)
      , xattr "tilecount" (show . tsTileCount $ tileset)
      ]
    tilesetChildren =
      [ tileToXml tile | tile <- tsTiles tileset ] ++
      images ++
      props
    images = imageElem <$> tsImages tileset
    imageElem img = xelem "image" $ xattrs
      [ xattr "source" (fromString . iSource $ img)
      , xattr "width" (show . iWidth $ img)
      , xattr "height" (show . iHeight $ img)
      ]
    props = if Prelude.null (tsProperties tileset)
            then []
            else [propertiesToXml $ tsProperties tileset]

xrenderString :: (Renderable r) => Xml r -> String
xrenderString = BS.unpack . xrender

propertiesToXml props =
  xelem "properties" $ xelems (fmap propertyToXml props)

propertyToXml (name,value) =
  xelem "property" $ xattrs
  [ xattr "name" (fromString name)
  , xattr "value" (fromString value)
  ]

tileToXml tile =
  xelem "tile" (tileAttrs,tileChildren)
  where
    tileAttrs = xattrs
      [ xattr "id" (show . tileId $ tile)
      ]
    tileChildren = xelems
      [ props
      ]
    props :: Xml Elem
    props = propertiesToXml (tileProperties tile)
