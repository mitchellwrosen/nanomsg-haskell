module Libnanomsg
  ( socket
  , version
  , Domain(..)
  , Operation(..)
  , Protocol(..)
  , Socket
  ) where

#include "nanomsg/nn.h"

import Internal
import Libnanomsg.Domain (Domain)
import Libnanomsg.Protocol (Protocol)

import qualified Libnanomsg.Domain as Domain
import qualified Libnanomsg.Protocol as Protocol

import Control.Exception (throwIO)
import Foreign.C

newtype Socket
  = Socket CInt

socket :: Domain -> Protocol -> IO (Either (Error NnSocket) Socket)
socket domain protocol = do
  fd :: CInt <-
    nn_socket (Domain.toCInt domain) (Protocol.toCInt protocol)

  if fd < 0
    then
      nn_errno >>= \case
        (#const EAFNOSUPPORT) -> pure (Left AddressFamilyNotSupported)
        (#const EINVAL)       -> pure (Left UnknownProtocol)
        (#const EMFILE)       -> pure (Left TooManyOpenFiles)
        (#const ETERM)        -> pure (Left Terminating)
        errno                 -> bug "socket" errno

    else
      pure (Right (Socket fd))

version :: (Int, Int)
version =
  (x, y)
  where
    x = #const NN_VERSION_CURRENT
    y = #const NN_VERSION_REVISION

foreign import ccall safe "nn.h nn_errno"
  nn_errno :: IO CInt

foreign import ccall safe "nn.h nn_socket"
  nn_socket :: CInt -> CInt -> IO CInt

foreign import ccall safe "nn.h nn_strerror"
  nn_strerror :: CInt -> IO CString

foreign import ccall safe "nn.h nn_term"
  nn_term :: IO ()

bug :: String -> CInt -> IO a
bug name errno =
  throwIO (userError msg)
  where
    msg :: String
    msg =
      concat
        [ "There is a bug in the `libnanomsg` library: the " ++ name
        , " function unexpectedly set errno to " ++ show errno ++ ". Please"
        , " file a bug immediately!"
        ]
