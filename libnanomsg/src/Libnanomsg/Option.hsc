module Libnanomsg.Option
  ( Option(..)
  , optionIpv4only
  , optionMaxttl
  , optionRcvbuf
  , optionRcvmaxsize
  , optionRcvprio
  , optionRcvtimeo
  , optionReconnectIvl
  , optionReconnectIvlMax
  , optionSndbuf
  , optionSndprio
  , optionSndtimeo
  ) where

#include "nanomsg/nn.h"

import Foreign.C

newtype Option
  = Option { unOption :: CInt }

optionDomain :: Option
optionDomain =
  Option (#const NN_DOMAIN)

optionIpv4only :: Option
optionIpv4only =
  Option (#const NN_IPV4ONLY)

optionMaxttl :: Option
optionMaxttl =
  Option (#const NN_MAXTTL)

optionProtocol :: Option
optionProtocol =
  Option (#const NN_PROTOCOL)

optionRcvbuf :: Option
optionRcvbuf =
  Option (#const NN_RCVBUF)

optionRcvfd :: Option
optionRcvfd =
  Option (#const NN_RCVFD)

optionRcvmaxsize :: Option
optionRcvmaxsize =
  Option (#const NN_RCVMAXSIZE)

optionRcvprio :: Option
optionRcvprio =
  Option (#const NN_RCVPRIO)

optionRcvtimeo :: Option
optionRcvtimeo =
  Option (#const NN_RCVTIMEO)

optionReconnectIvl :: Option
optionReconnectIvl =
  Option (#const NN_RECONNECT_IVL)

optionReconnectIvlMax :: Option
optionReconnectIvlMax =
  Option (#const NN_RECONNECT_IVL_MAX)

optionSndbuf :: Option
optionSndbuf =
  Option (#const NN_SNDBUF)

optionSndfd :: Option
optionSndfd =
  Option (#const NN_SNDFD)

optionSndprio :: Option
optionSndprio =
  Option (#const NN_SNDPRIO)

optionSndtimeo :: Option
optionSndtimeo =
  Option (#const NN_SNDTIMEO)
