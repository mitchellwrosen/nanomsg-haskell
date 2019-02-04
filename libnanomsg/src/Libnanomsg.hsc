{-# OPTIONS_GHC -fno-warn-name-shadowing                 #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Libnanomsg
  ( Address(..)
  , bind
  , close
  , connect
  , getIPv4Only
  , getMaxReconnectInterval
  , getMaxRecvSize
  , getMaxTTL
  , getReconnectInterval
  , getRecvBufferSize
  , getRecvPriority
  , getRecvTimeout
  , getSendBufferSize
  , getSendPriority
  , getSendTimeout
  , recv
  , send
  , setIPv4Only
  , setMaxReconnectInterval
  , setMaxRecvSize
  , setMaxTTL
  , setReconnectInterval
  , setRecvBufferSize
  , setRecvPriority
  , setRecvTimeout
  , setSendBufferSize
  , setSendPriority
  , setSendTimeout
  , shutdown
  , socket
  , version
  , Address(..)
  , Endpoint
  , Domain
  , domainSp
  , domainSpRaw
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
  ) where

#include "nanomsg/nn.h"

import Libnanomsg.Address (Address(..))
import Libnanomsg.Domain
import Libnanomsg.FFI
import Libnanomsg.Level
import Libnanomsg.Option
import Libnanomsg.Protocol
import Libnanomsg.RecvFlags
import Libnanomsg.SendFlags

import qualified Libnanomsg.Address as Address

import Data.ByteString (ByteString)
import Data.Primitive.Addr (Addr(..))
import Data.Text (Text)
import Foreign.C
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke, sizeOf)

import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Text.Encoding as Text

newtype Endpoint
  = Endpoint CInt
  deriving (Eq, Show)

newtype Socket
  = Socket CInt
  deriving (Eq, Show)

-- | <https://nanomsg.org/v1.1.5/nn_bind.html>
bind :: Socket -> Address -> IO (Either Errno Endpoint)
bind (Socket fd) address =
  Address.asCString address $ \caddr ->
    nn_bind fd caddr >>= \case
      -1 ->
        Left <$> getErrno

      endpoint ->
        pure (Right (Endpoint endpoint))

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
connect :: Socket -> Address -> IO (Either Errno Endpoint)
connect (Socket fd) address =
  Address.asCString address $ \caddr -> do
    endpoint :: CInt <-
      nn_connect fd caddr

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
  nn_getsockopt fd (unLevel level) (unOption option) value size >>= \case
    0 ->
      pure (Right ())

    _ ->
      Left <$> getErrno

getsockopt_int ::
     Socket
  -> Level
  -> Option
  -> IO (Either Errno CInt)
getsockopt_int socket level option =
  alloca $ \valuePtr ->
  alloca $ \sizePtr ->
    getsockopt socket level option valuePtr sizePtr >>= \case
      Left errno -> pure (Left errno)
      Right () -> Right <$> peek valuePtr

getIPv4Only ::
     Socket
  -> IO (Either Errno Bool)
getIPv4Only socket =
  (fmap.fmap)
    (\n -> if n == 0 then False else True)
    (getsockopt_int socket levelSocket optionIpv4only)

getMaxReconnectInterval ::
     Socket
  -> IO (Either Errno CInt)
getMaxReconnectInterval socket =
  getsockopt_int socket levelSocket optionReconnectIvlMax

getMaxRecvSize ::
     Socket
  -> IO (Either Errno CInt)
getMaxRecvSize socket =
  getsockopt_int socket levelSocket optionRcvmaxsize

getMaxTTL ::
     Socket
  -> IO (Either Errno CInt)
getMaxTTL socket =
  getsockopt_int socket levelSocket optionMaxttl

getReconnectInterval ::
     Socket
  -> IO (Either Errno CInt)
getReconnectInterval socket =
  getsockopt_int socket levelSocket optionReconnectIvl

getRecvBufferSize ::
     Socket
  -> IO (Either Errno CInt)
getRecvBufferSize socket =
  getsockopt_int socket levelSocket optionRcvbuf

getRecvPriority ::
     Socket
  -> IO (Either Errno CInt)
getRecvPriority socket =
  getsockopt_int socket levelSocket optionRcvprio

getRecvTimeout ::
     Socket
  -> IO (Either Errno CInt)
getRecvTimeout socket =
  getsockopt_int socket levelSocket optionRcvtimeo

getSendBufferSize ::
     Socket
  -> IO (Either Errno CInt)
getSendBufferSize socket =
  getsockopt_int socket levelSocket optionSndbuf

getSendPriority ::
     Socket
  -> IO (Either Errno CInt)
getSendPriority socket =
  getsockopt_int socket levelSocket optionSndprio

getSendTimeout ::
     Socket
  -> IO (Either Errno CInt)
getSendTimeout socket =
  getsockopt_int socket levelSocket optionSndtimeo

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
  nn_setsockopt fd (unLevel level) (unOption option) value len >>= \case
    0 ->
      pure (Right ())

    _ ->
      Left <$> getErrno

setsockopt_int ::
     Socket
  -> Level
  -> Option
  -> CInt
  -> IO (Either Errno ())
setsockopt_int socket level option value =
  alloca $ \ptr -> do
    poke ptr value
    setsockopt socket level option ptr (fromIntegral (sizeOf (undefined :: CInt)))

setIPv4Only ::
     Socket
  -> Bool
  -> IO (Either Errno ())
setIPv4Only socket value =
  setsockopt_int socket levelSocket optionIpv4only (if value then 1 else 0)

setMaxReconnectInterval ::
     Socket
  -> CInt
  -> IO (Either Errno ())
setMaxReconnectInterval socket =
  setsockopt_int socket levelSocket optionReconnectIvlMax

setMaxRecvSize ::
     Socket
  -> CInt
  -> IO (Either Errno ())
setMaxRecvSize socket =
  setsockopt_int socket levelSocket optionRcvmaxsize

setMaxTTL ::
     Socket
  -> CInt
  -> IO (Either Errno ())
setMaxTTL socket =
  setsockopt_int socket levelSocket optionMaxttl

setReconnectInterval ::
     Socket
  -> CInt
  -> IO (Either Errno ())
setReconnectInterval socket =
  setsockopt_int socket levelSocket optionReconnectIvl

setRecvBufferSize ::
     Socket
  -> CInt
  -> IO (Either Errno ())
setRecvBufferSize socket =
  setsockopt_int socket levelSocket optionRcvbuf

setRecvPriority ::
     Socket
  -> CInt
  -> IO (Either Errno ())
setRecvPriority socket =
  setsockopt_int socket levelSocket optionRcvprio

setRecvTimeout ::
     Socket
  -> CInt
  -> IO (Either Errno ())
setRecvTimeout socket =
  setsockopt_int socket levelSocket optionRcvtimeo

setSendBufferSize ::
     Socket
  -> CInt
  -> IO (Either Errno ())
setSendBufferSize socket =
  setsockopt_int socket levelSocket optionSndbuf

setSendPriority ::
     Socket
  -> CInt
  -> IO (Either Errno ())
setSendPriority socket =
  setsockopt_int socket levelSocket optionSndprio

setSendTimeout ::
     Socket
  -> CInt
  -> IO (Either Errno ())
setSendTimeout socket =
  setsockopt_int socket levelSocket optionSndtimeo

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
