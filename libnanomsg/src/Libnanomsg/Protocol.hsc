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
  = BUS
  | PAIR
  | PUB
  | PULL
  | PUSH
  | REP
  | REQ
  | RESPONDENT
  | SUB
  | SURVEYOR

toCInt :: Protocol -> CInt
toCInt = \case
  BUS        -> #const NN_BUS
  PAIR       -> #const NN_PAIR
  PUB        -> #const NN_PUB
  PULL       -> #const NN_PULL
  PUSH       -> #const NN_PUSH
  REP        -> #const NN_REP
  REQ        -> #const NN_REQ
  RESPONDENT -> #const NN_RESPONDENT
  SUB        -> #const NN_SUB
  SURVEYOR   -> #const NN_SURVEYOR
