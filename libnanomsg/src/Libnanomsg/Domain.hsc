module Libnanomsg.Domain
  ( Domain(..)
  , domainSp
  , domainSpRaw
  ) where

#include "nanomsg/nn.h"

import Foreign.C

newtype Domain
  = Domain { unDomain :: CInt }

domainSp :: Domain
domainSp =
  Domain (#const AF_SP)

domainSpRaw :: Domain
domainSpRaw =
  Domain (#const AF_SP_RAW)
