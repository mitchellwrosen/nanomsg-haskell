module Libnanomsg.Level
  ( Level(..)
  , levelSocket
  ) where

#include "nanomsg/nn.h"

import Foreign.C

newtype Level
  = Level { unLevel :: CInt }

levelSocket :: Level
levelSocket =
  Level (#const NN_SOL_SOCKET)
