module Libnanomsg.RecvFlags where

#include "nanomsg/nn.h"

import Data.Bits
import Foreign.C

newtype RecvFlags
  = RecvFlags { unRecvFlags :: CInt }

instance Monoid RecvFlags where
  mempty = RecvFlags 0
  mappend = (<>)

instance Semigroup RecvFlags where
  RecvFlags x <> RecvFlags y =
    RecvFlags (x .|. y)

recvFlagDontwait :: RecvFlags
recvFlagDontwait =
  RecvFlags (#const NN_DONTWAIT)
