{-# LANGUAGE ForeignFunctionInterface #-}

module Hikchr (
  hikchr,
  hikchrCustom,
  HikchrConfig (..),
  defaultConfig,
) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Data.ByteString.Char8 qualified as BS
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, sizeOf)


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
  { svgClass :: Maybe Text
  , darkMode :: Bool
  , width :: Maybe Int
  , height :: Maybe Int
  }


defaultConfig :: HikchrConfig
defaultConfig =
  HikchrConfig
    { svgClass = Nothing
    , darkMode = False
    , width = Nothing
    , height = Nothing
    }


hikchr :: Text -> IO (Either Text Text)
hikchr =
  hikchrCustom defaultConfig


withClassMaybe :: Maybe Text -> (CString -> IO a) -> IO a
withClassMaybe Nothing f = f nullPtr
withClassMaybe (Just cls) f = BS.useAsCString (encodeUtf8 cls) f


hikchrCustom :: HikchrConfig -> Text -> IO (Either Text Text)
hikchrCustom config pikchrScript = do
  BS.useAsCString (encodeUtf8 pikchrScript) $ \scriptStr ->
    withClassMaybe config.svgClass $ \classStrPtr ->
      allocaBytes (sizeOf (undefined :: CInt)) $ \widthPtr ->
        allocaBytes (sizeOf (undefined :: CInt)) $ \heightPtr -> do
          let
            -- PIKCHR_PLAINTEXT_ERRORS is always set
            flags = if config.darkMode then 3 else 1

          result <- pikchr scriptStr classStrPtr flags widthPtr heightPtr
          resCode <- peek widthPtr
          resultBS <- BS.packCString result

          return $
            if resCode < 0
              then Left $ decodeUtf8 resultBS
              else Right $ decodeUtf8 resultBS
