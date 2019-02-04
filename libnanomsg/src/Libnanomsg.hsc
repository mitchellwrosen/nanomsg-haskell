{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Libnanomsg
  ( bind
  , close
  , connect
  , getsockopt
  , recv
  , send
  , setsockopt
  , shutdown
  , socket
  , version
  , Domain
  , domainSp
  , domainSpRaw
  , Level
  , levelSocket
  , Protocol
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
  , RecvFlags
  , recvFlagDontwait
  , SendFlags
  , sendFlagDontwait
  , Socket
  , Transport(..)
  ) where

#include "nanomsg/nn.h"

import Libnanomsg.Domain
import Libnanomsg.FFI
import Libnanomsg.Level
import Libnanomsg.Option (Option)
import Libnanomsg.Protocol
import Libnanomsg.RecvFlags
import Libnanomsg.SendFlags
import Libnanomsg.Transport (Transport)

import qualified Libnanomsg.Option as Option
import qualified Libnanomsg.Transport as Transport

import Data.ByteString (ByteString)
import Data.Primitive.Addr (Addr(..))
import Data.Text (Text)
import Foreign.C
import Foreign.Ptr (Ptr)

import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Text.Encoding as Text

newtype Endpoint
  = Endpoint CInt

newtype Socket
  = Socket CInt

-- | <https://nanomsg.org/v1.1.5/nn_bind.html>
bind :: Socket -> Transport -> Text -> IO (Either Errno Endpoint)
bind (Socket fd) transport addr =
  ByteString.unsafeUseAsCString addrBytes $ \caddr ->
    nn_bind fd caddr >>= \case
      -1 ->
        Left <$> getErrno

      endpoint ->
        pure (Right (Endpoint endpoint))

  where
    addrBytes :: ByteString
    addrBytes =
      Text.encodeUtf8 (Transport.toText transport <> addr <> "\0")

-- | <https://nanomsg.org/v1.1.5/nn_close.html>
close :: Socket -> IO (Either Errno ())
close (Socket fd) =
  go

  where
    go :: IO (Either Errno ())
    go =
      nn_close fd >>= \case
        0 ->
          pure (Right ())

        _ -> do
          errno <- getErrno
          if errno == eINTR
            then go
            else pure (Left errno)

-- | <https://nanomsg.org/v1.1.5/nn_connect.html>
connect :: Socket -> CString -> IO (Either Errno Endpoint)
connect (Socket fd) addr = do
  endpoint :: CInt <-
    nn_connect fd addr

  if endpoint < 0
    then Left <$> getErrno
    else pure (Right (Endpoint endpoint))

-- | https://nanomsg.org/v1.1.5/nn_getsockopt.html
getsockopt ::
     Socket
  -> Level
  -> Option
  -> Ptr a
  -> Ptr CSize
  -> IO (Either Errno ())
getsockopt (Socket fd) level option value size =
  nn_getsockopt fd (unLevel level) (Option.toCInt option) value size >>= \case
    0 ->
      pure (Right ())

    _ ->
      Left <$> getErrno

-- | <https://nanomsg.org/v1.1.5/nn_recv.html>
recv ::
     Socket
  -> Addr
  -> CSize
  -> RecvFlags
  -> IO (Either Errno CInt)
recv (Socket fd) (Addr addr) len flags =
  nn_recv fd addr len (unRecvFlags flags) >>= \case
    -1 ->
      Left <$> getErrno

    received ->
      pure (Right received)

-- | <https://nanomsg.org/v1.1.5/nn_send.html>
send ::
     Socket
  -> Addr
  -> CSize
  -> SendFlags
  -> IO (Either Errno CInt)
send (Socket fd) (Addr addr) len flags =
  nn_send fd addr len (unSendFlags flags) >>= \case
    -1 ->
      Left <$> getErrno

    sent ->
      pure (Right sent)

-- | <https://nanomsg.org/v1.1.5/nn_setsockopt.html>
setsockopt ::
     Socket
  -> Level
  -> Option
  -> Ptr a
  -> CSize
  -> IO (Either Errno ())
setsockopt (Socket fd) level option value len =
  nn_setsockopt fd (unLevel level) (Option.toCInt option) value len >>= \case
    0 ->
      pure (Right ())

    _ ->
      Left <$> getErrno

-- | <https://nanomsg.org/v1.1.5/nn_shutdown.html>
shutdown :: Socket -> Endpoint -> IO (Either Errno ())
shutdown (Socket fd) (Endpoint endpoint) =
  go

  where
    go :: IO (Either Errno ())
    go =
      nn_shutdown fd endpoint >>= \case
        0 -> pure (Right ())
        _ -> do
          errno <- getErrno
          if errno == eINTR
            then go
            else pure (Left errno)

-- | <https://nanomsg.org/v1.1.5/nn_socket.html>
socket :: Domain -> Protocol -> IO (Either Errno Socket)
socket domain protocol = do
  fd :: CInt <-
    nn_socket (unDomain domain) (unProtocol protocol)

  if fd < 0
    then Left <$> getErrno
    else pure (Right (Socket fd))

version :: (Int, Int)
version =
  (x, y)
  where
    x = #const NN_VERSION_CURRENT
    y = #const NN_VERSION_REVISION
