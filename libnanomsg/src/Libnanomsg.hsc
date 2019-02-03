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
import Libnanomsg.Level (Level)
import Libnanomsg.Option (Option)
import Libnanomsg.Protocol (Protocol)

import qualified Libnanomsg.Domain as Domain
import qualified Libnanomsg.Level as Level
import qualified Libnanomsg.Option as Option
import qualified Libnanomsg.Protocol as Protocol

import Control.Exception (throwIO)
import Foreign.C
import Foreign.Ptr (Ptr)

newtype Socket
  = Socket CInt

-- | <https://nanomsg.org/v1.1.5/nn_close.html>
close :: Socket -> IO (Either (Error NnClose) ())
close (Socket fd) =
  go

  where
    go :: IO (Either (Error NnClose) ())
    go =
      nn_close fd >>= \case
        0 ->
          pure (Right ())

        _ ->
          nn_errno >>= \case
            EBADF -> pure (Left InvalidSocket)
            EINTR -> go
            errno -> bug "close" errno


-- | https://nanomsg.org/v1.1.5/nn_getsockopt.html
getsockopt ::
     Socket
  -> Level
  -> Option
  -> Ptr a
  -> Ptr CSize
  -> IO (Either (Error NnGetsockopt) ())
getsockopt (Socket fd) level option value size =
  nn_getsockopt fd (Level.toCInt level) (Option.toCInt option) value size >>= \case
    0 ->
      pure (Right ())

    _ ->
      nn_errno >>= \case
        EBADF -> pure (Left InvalidSocket)
        ENOPROTOOPT -> pure (Left InvalidProtocol)
        ETERM -> pure (Left Terminating)
        errno -> bug "getsockopt" errno

-- | <https://nanomsg.org/v1.1.5/nn_setsockopt.html>
setsockopt ::
     Socket
  -> Level
  -> Option
  -> Ptr a
  -> CSize
  -> IO (Either (Error NnSetsockopt) ())
setsockopt (Socket fd) level option value len =
  nn_setsockopt fd (Level.toCInt level) (Option.toCInt option) value len >>= \case
    0 ->
      pure (Right ())

    _ ->
      nn_errno >>= \case
        EBADF -> pure (Left InvalidSocket)
        ENOPROTOOPT -> pure (Left InvalidProtocol)
        EINVAL -> pure (Left InvalidOption)
        ETERM -> pure (Left Terminating)
        errno -> bug "setsockopt" errno

-- | <https://nanomsg.org/v1.1.5/nn_socket.html>
socket :: Domain -> Protocol -> IO (Either (Error NnSocket) Socket)
socket domain protocol = do
  fd :: CInt <-
    nn_socket (Domain.toCInt domain) (Protocol.toCInt protocol)

  if fd < 0
    then
      nn_errno >>= \case
        EAFNOSUPPORT -> pure (Left AddressFamilyNotSupported)
        EINVAL -> pure (Left InvalidProtocol)
        EMFILE -> pure (Left TooManyOpenFiles)
        ETERM -> pure (Left Terminating)
        errno -> bug "socket" errno

    else
      pure (Right (Socket fd))

version :: (Int, Int)
version =
  (x, y)
  where
    x = #const NN_VERSION_CURRENT
    y = #const NN_VERSION_REVISION

pattern EAFNOSUPPORT :: CInt
pattern EAFNOSUPPORT = #const EAFNOSUPPORT

pattern EBADF :: CInt
pattern EBADF = #const EBADF

pattern EINTR :: CInt
pattern EINTR = #const EINTR

pattern EINVAL :: CInt
pattern EINVAL = #const EINVAL

pattern EMFILE :: CInt
pattern EMFILE = #const EMFILE

pattern ENOPROTOOPT :: CInt
pattern ENOPROTOOPT = #const ENOPROTOOPT

pattern ETERM :: CInt
pattern ETERM = #const ETERM

foreign import ccall safe "nn.h nn_close"
  nn_close :: CInt -> IO CInt

foreign import ccall safe "nn.h nn_errno"
  nn_errno :: IO CInt

foreign import ccall safe "nn.h nn_getsockopt"
  nn_getsockopt :: CInt -> CInt -> CInt -> Ptr a -> Ptr CSize -> IO CInt

foreign import ccall safe "nn.h nn_setsockopt"
  nn_setsockopt :: CInt -> CInt -> CInt -> Ptr a -> CSize -> IO CInt

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
