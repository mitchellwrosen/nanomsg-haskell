module Libnanomsg.Option
  ( Option(..)
  , toCInt
  ) where

#include "nanomsg/nn.h"

import Foreign.C

data Option
  = DOMAIN
  | IPV4ONLY
  | LINGER
  | MAXTTL
  | PROTOCOL
  | RCVBUF
  | RCVFD
  | RCVMAXSIZE
  | RCVPRIO
  | RCVTIMEO
  | RECONNECT_IVL
  | RECONNECT_IVL_MAX
  | SNDBUF
  | SNDFD
  | SNDPRIO
  | SNDTIMEO
  | SOCKET_NAME

toCInt :: Option -> CInt
toCInt = \case
  DOMAIN -> 12
  IPV4ONLY -> 14
  LINGER -> 1
  MAXTTL -> 17
  PROTOCOL -> 13
  RCVBUF -> 3
  RCVFD -> 11
  RCVMAXSIZE -> 16
  RCVPRIO -> 9
  RCVTIMEO -> 5
  RECONNECT_IVL -> 6
  RECONNECT_IVL_MAX -> 7
  SNDBUF -> 2
  SNDFD -> 10
  SNDPRIO -> 8
  SNDTIMEO -> 4
  SOCKET_NAME -> 15
