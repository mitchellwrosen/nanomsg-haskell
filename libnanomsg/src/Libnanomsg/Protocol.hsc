module Libnanomsg.Protocol
  ( Protocol(..)
  , protocolBus
  , protocolPair
  , protocolPub
  , protocolPull
  , protocolPush
  , protocolRep
  , protocolReq
  , protocolRespondent
  , protocolSub
  , protocolSurveyor
  ) where

#include "nanomsg/bus.h"
#include "nanomsg/pair.h"
#include "nanomsg/pipeline.h"
#include "nanomsg/pubsub.h"
#include "nanomsg/reqrep.h"
#include "nanomsg/survey.h"

import Foreign.C

newtype Protocol
  = Protocol { unProtocol :: CInt }

protocolBus :: Protocol
protocolBus =
  Protocol (#const NN_BUS)

protocolPair :: Protocol
protocolPair =
  Protocol (#const NN_PAIR)

protocolPub :: Protocol
protocolPub =
  Protocol (#const NN_PUB)

protocolPull :: Protocol
protocolPull =
  Protocol (#const NN_PULL)

protocolPush :: Protocol
protocolPush =
  Protocol (#const NN_PUSH)

protocolRep :: Protocol
protocolRep =
  Protocol (#const NN_REP)

protocolReq :: Protocol
protocolReq =
  Protocol (#const NN_REQ)

protocolRespondent :: Protocol
protocolRespondent =
  Protocol (#const NN_RESPONDENT)

protocolSub :: Protocol
protocolSub =
  Protocol (#const NN_SUB)

protocolSurveyor :: Protocol
protocolSurveyor =
  Protocol (#const NN_SURVEYOR)
