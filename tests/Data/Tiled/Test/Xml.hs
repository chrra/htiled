{-# LANGUAGE OverloadedStrings #-}

module Data.Tiled.Test.Xml where

import           Data.ByteString.Lazy.Char8 as BS
import           Data.Char
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

layerToXml l@(Layer {layerContents = LayerContentsImage i}) =
  xelem "imagelayer" (layerAttrs,layerChildren)
  where
    layerAttrs = xattrs
      [ xattr "name" (fromString . layerName $ l)
      , xattr "offsetx" (show . fst . layerOffset $ l)
      , xattr "offsety" (show . snd . layerOffset $ l)
      , xattr "opacity" (show . layerOpacity $ l)
      , xattr "visible" (show . convertVisible . layerIsVisible $ l)
      ]
    layerChildren = xelems
      [ imageToXml i
      , propertiesToXml (layerProperties l)
      ]
    convertVisible True = 1
    convertVisible False = 0

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
    images = imageToXml <$> tsImages tileset
    props = if Prelude.null (tsProperties tileset)
            then []
            else [propertiesToXml $ tsProperties tileset]

imageToXml img =  xelem "image" $ xattrs
  [ xattr "source" (fromString . iSource $ img)
  , xattr "width" (show . iWidth $ img)
  , xattr "height" (show . iHeight $ img)
  ]

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
    tileChildren = xelems $
      [ props
      ] ++ img
    props :: Xml Elem
    props = propertiesToXml (tileProperties tile)
    img :: [Xml Elem]
    img = case tileImage tile of
      Nothing -> []
      Just i ->
        let
          trans = case iTrans i of
            Nothing -> []
            Just (r,g,b) ->
              [ xattr "trans"
                (fromString $
                 showAsHex (fromIntegral r) ++
                 showAsHex (fromIntegral g) ++
                 showAsHex (fromIntegral b)
                )
              ]
        in
          [ xelem "image" $ xattrs $
            [ xattr "width" (show . iWidth $ i)
            , xattr "height" (show . iHeight $ i)
            , xattr "source" (fromString . iSource $ i)
            ] ++ trans
          ]

showAsHex n
  | n < 0 = error "showAsHex not implemented for negative numbers"
  | n == 0 = "0"
  | otherwise =
    (if (n `div` 16) > 0 then showAsHex (n `div` 16) else []) ++
    [intToDigit (n `mod` 16)]
