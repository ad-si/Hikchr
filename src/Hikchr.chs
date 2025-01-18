{-# LANGUAGE ForeignFunctionInterface #-}

module Hikchr (hikchr, HikchrConfig(..)) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Foreign.C.String (CString)
import qualified Data.ByteString.Char8 as BS
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, nullPtr)

#include "pikchr.h"

{#fun pikchr
  { `CString'     -- ^ Input PIKCHR source text
  , `CString'     -- ^ Add class="%s" to <svg> markup
  , `CInt'        -- ^ Flags used to influence rendering behavior
  , id `Ptr CInt' -- ^ `width` of <svg> or `NULL`
  , id `Ptr CInt' -- ^ `height` or `NULL`
  } -> `CString'  -- ^ SVG markup
#}


data HikchrConfig = HikchrConfig
      { diagram :: Text
      , svgClass :: Text
      , renderFlags :: Int
      , width :: Maybe Int
      , height :: Maybe Int
      }


hikchr :: HikchrConfig -> IO Text
hikchr config = do
  BS.useAsCString (encodeUtf8 config.diagram) $ \diagramStr ->
    BS.useAsCString (encodeUtf8 config.svgClass) $ \classStr -> do
      result <- pikchr
        diagramStr
        classStr
        (fromIntegral config.renderFlags)
        nullPtr  -- width pointer not used
        nullPtr  -- height pointer not used
      resultBS <- BS.packCString result
      return $ decodeUtf8 resultBS
