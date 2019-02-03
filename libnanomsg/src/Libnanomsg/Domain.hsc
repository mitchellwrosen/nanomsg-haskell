module Libnanomsg.Domain
  ( Domain(..)
  , toCInt
  ) where

#include "nanomsg/nn.h"

import Foreign.C

data Domain
  = SP
  | SP_RAW

toCInt :: Domain -> CInt
toCInt = \case
  SP -> #const AF_SP
  SP_RAW -> #const AF_SP_RAW
