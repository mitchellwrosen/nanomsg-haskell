module Libnanomsg.Domain
  ( Domain(..)
  , toCInt
  ) where

#include "nanomsg/nn.h"

import Foreign.C

data Domain
  = AF_SP
  | AF_SP_RAW

toCInt :: Domain -> CInt
toCInt = \case
  AF_SP -> #const AF_SP
  AF_SP_RAW -> #const AF_SP_RAW
