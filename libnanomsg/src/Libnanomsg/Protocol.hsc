module Libnanomsg.Protocol
  ( Protocol(..)
  , toCInt
  ) where

#include "nanomsg/bus.h"
#include "nanomsg/pair.h"
#include "nanomsg/pipeline.h"
#include "nanomsg/pubsub.h"
#include "nanomsg/reqrep.h"
#include "nanomsg/survey.h"

import Foreign.C

data Protocol
  = Bus
  | Pair
  | Pub
  | Pull
  | Push
  | Rep
  | Req
  | Respondent
  | Sub
  | Surveyor

toCInt :: Protocol -> CInt
toCInt = \case
  Bus        -> #const NN_BUS
  Pair       -> #const NN_PAIR
  Pub        -> #const NN_PUB
  Pull       -> #const NN_PULL
  Push       -> #const NN_PUSH
  Rep        -> #const NN_REP
  Req        -> #const NN_REQ
  Respondent -> #const NN_RESPONDENT
  Sub        -> #const NN_SUB
  Surveyor   -> #const NN_SURVEYOR
