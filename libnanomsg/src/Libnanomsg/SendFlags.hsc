module Libnanomsg.SendFlags where

#include "nanomsg/nn.h"

import Data.Bits
import Foreign.C

newtype SendFlags
  = SendFlags { unSendFlags :: CInt }

instance Monoid SendFlags where
  mempty = SendFlags 0
  mappend = (<>)

instance Semigroup SendFlags where
  SendFlags x <> SendFlags y =
    SendFlags (x .|. y)

sendFlagDontwait :: SendFlags
sendFlagDontwait =
  SendFlags (#const NN_DONTWAIT)
