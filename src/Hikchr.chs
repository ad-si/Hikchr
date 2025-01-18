module Hikchr where

import Protolude (
  Double,
  fromIntegral,
  identity,
  Int,
  IO,
  Bool,
  Ptr,
  realToFrac,
  Show,
  return,
  (*),
  (>>=),
 )

import Foreign.C.Types (CUChar)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

#include "pikchr.h"

{#fun pikchr
  {               `Ptr CUChar' -- ^ Input PIKCHR source text
  ,               `Ptr CUChar' -- ^ Add class="%s" to <svg> markup
  ,               `Int'        -- ^ Flags used to influence rendering behavior
  , castPtr       `Ptr Int'    -- ^ `width` of <svg> or `NULL`
  , castPtr       `Ptr Int'    -- ^ `height` or `NULL`
  } -> `Ptr CUChar' castPtr    -- ^ SVG markup
#}
