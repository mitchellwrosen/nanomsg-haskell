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

newtype Endpoint
  = Endpoint CInt

newtype Socket
  = Socket CInt

-- | <https://nanomsg.org/v1.1.5/nn_bind.html>
bind :: Socket -> CString -> IO (Either (Error NnBind) Endpoint)
bind (Socket fd) addr = do
  nn_bind fd addr >>= \case
    -1 ->
      nn_errno >>= \case
        EADDRINUSE_      -> pure (Left EADDRINUSE)
        EADDRNOTAVAIL_   -> pure (Left EADDRNOTAVAIL)
        EBADF_           -> pure (Left EBADF)
        EINVAL_          -> pure (Left EINVAL)
        EMFILE_          -> pure (Left EMFILE)
        ENAMETOOLONG_    -> pure (Left ENAMETOOLONG)
        ENODEV_          -> pure (Left ENODEV)
        EPROTONOSUPPORT_ -> pure (Left EPROTONOSUPPORT)
        ETERM_           -> pure (Left ETERM)
        errno            -> bug "bind" errno

    endpointId ->
      pure (Right (Endpoint endpointId))

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
            EBADF_ -> pure (Left EBADF)
            EINTR_ -> go
            errno  -> bug "close" errno

-- | <https://nanomsg.org/v1.1.5/nn_connect.html>
connect :: Socket -> CString -> IO (Either (Error NnConnect) Endpoint)
connect (Socket fd) addr = do
  endpointId :: CInt <-
    nn_connect fd addr

  if endpointId < 0
    then
      nn_errno >>= \case
        EBADF_           -> pure (Left EBADF)
        EMFILE_          -> pure (Left EMFILE)
        EINVAL_          -> pure (Left EINVAL)
        ENAMETOOLONG_    -> pure (Left ENAMETOOLONG)
        EPROTONOSUPPORT_ -> pure (Left EPROTONOSUPPORT)
        ENODEV_          -> pure (Left ENODEV)
        ETERM_           -> pure (Left ETERM)
        errno            -> bug "connect" errno
    else
      pure (Right (Endpoint endpointId))

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
        EBADF_       -> pure (Left EBADF)
        ENOPROTOOPT_ -> pure (Left ENOPROTOOPT)
        ETERM_       -> pure (Left ETERM)
        errno        -> bug "getsockopt" errno

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
        EBADF_       -> pure (Left EBADF)
        EINVAL_      -> pure (Left EINVAL)
        ENOPROTOOPT_ -> pure (Left ENOPROTOOPT)
        ETERM_       -> pure (Left ETERM)
        errno        -> bug "setsockopt" errno

-- | <https://nanomsg.org/v1.1.5/nn_socket.html>
socket :: Domain -> Protocol -> IO (Either (Error NnSocket) Socket)
socket domain protocol = do
  fd :: CInt <-
    nn_socket (Domain.toCInt domain) (Protocol.toCInt protocol)

  if fd < 0
    then
      nn_errno >>= \case
        EAFNOSUPPORT_ -> pure (Left EAFNOSUPPORT)
        EINVAL_       -> pure (Left EINVAL)
        EMFILE_       -> pure (Left EMFILE)
        ETERM_        -> pure (Left ETERM)
        errno         -> bug "socket" errno

    else
      pure (Right (Socket fd))

version :: (Int, Int)
version =
  (x, y)
  where
    x = #const NN_VERSION_CURRENT
    y = #const NN_VERSION_REVISION

pattern EADDRINUSE_ :: CInt
pattern EADDRINUSE_ = #const EADDRINUSE

pattern EADDRNOTAVAIL_ :: CInt
pattern EADDRNOTAVAIL_ = #const EADDRNOTAVAIL

pattern EAFNOSUPPORT_ :: CInt
pattern EAFNOSUPPORT_ = #const EAFNOSUPPORT

pattern EBADF_ :: CInt
pattern EBADF_ = #const EBADF

pattern EINTR_ :: CInt
pattern EINTR_ = #const EINTR

pattern EINVAL_ :: CInt
pattern EINVAL_ = #const EINVAL

pattern EPROTONOSUPPORT_ :: CInt
pattern EPROTONOSUPPORT_ = #const EPROTONOSUPPORT

pattern EMFILE_ :: CInt
pattern EMFILE_ = #const EMFILE

pattern ENAMETOOLONG_ :: CInt
pattern ENAMETOOLONG_ = #const ENAMETOOLONG

pattern ENODEV_ :: CInt
pattern ENODEV_ = #const ENODEV

pattern ENOPROTOOPT_ :: CInt
pattern ENOPROTOOPT_ = #const ENOPROTOOPT

pattern ETERM_ :: CInt
pattern ETERM_ = #const ETERM

foreign import ccall safe "nn.h nn_bind"
  nn_bind :: CInt -> CString -> IO CInt

foreign import ccall safe "nn.h nn_close"
  nn_close :: CInt -> IO CInt

foreign import ccall safe "nn.h nn_connect"
  nn_connect :: CInt -> CString -> IO CInt

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
