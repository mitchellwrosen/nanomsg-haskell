module Libnanomsg.Level
  ( Level(..)
  , toCInt
  ) where

#include "nanomsg/nn.h"

import Foreign.C

data Level
  = SOCKET

toCInt :: Level -> CInt
toCInt = \case
  SOCKET -> #const NN_SOL_SOCKET
