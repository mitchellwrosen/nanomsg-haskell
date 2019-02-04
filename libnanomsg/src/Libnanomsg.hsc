{-# OPTIONS_GHC -fno-warn-name-shadowing                 #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Libnanomsg
  ( -- * Socket create/destroy
    socket
  , close

    -- * Endpoint manipulation
  , bind
  , connect
  , shutdown

    -- * Messaging
  , send
  , recv

    -- * Socket options
    -- ** Get socket option
  , ipv4Only
  , maxReconnectInterval
  , maxRecvSize
  , maxTTL
  , reconnectInterval
  , recvBufferSize
  , recvFd
  , recvPriority
  , recvTimeout
  , sendBufferSize
  , sendFd
  , sendPriority
  , sendTimeout
    -- ** Set socket option
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

    -- * Socket statistics
    -- ** Connections
  , acceptedConnections
  , brokenConnections
  , currentConnections
  , droppedConnections
  , establishedConnections
    -- ** Errors
  , acceptErrors
  , bindErrors
  , connectErrors
    -- ** Throughput
  , messagesSent
  , messagesReceived
  , bytesSent
  , bytesReceived

    -- * Error reporting
  , strerror

    -- * Teardown
  , term

    -- * Nanomsg version
  , version

    -- * Misc. TODO Figure out how to organize these
  , device

    -- * Types
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

import Data.Coerce (coerce)
import Data.Primitive.Addr (Addr(..))
import Data.Text (Text)
import Data.Void (Void)
import Foreign.C
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke, sizeOf)
import System.Posix.Types (Fd(..))

import qualified Data.Text as Text


newtype Endpoint
  = Endpoint CInt
  deriving (Eq, Show)

newtype Socket
  = Socket CInt
  deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Socket create/destroy
--------------------------------------------------------------------------------

-- | <https://nanomsg.org/v1.1.5/nn_socket.html>
socket :: Domain -> Protocol -> IO (Either Errno Socket)
socket domain protocol = do
  fd :: CInt <-
    nn_socket (unDomain domain) (unProtocol protocol)

  if fd < 0
    then Left <$> getErrno
    else pure (Right (Socket fd))

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


--------------------------------------------------------------------------------
-- Endpoint manipulation
--------------------------------------------------------------------------------

-- | <https://nanomsg.org/v1.1.5/nn_bind.html>
bind :: Socket -> Address -> IO (Either Errno Endpoint)
bind (Socket fd) address =
  Address.asCString address $ \caddr ->
    nn_bind fd caddr >>= \case
      -1 ->
        Left <$> getErrno

      endpoint ->
        pure (Right (Endpoint endpoint))

-- | <https://nanomsg.org/v1.1.5/nn_connect.html>
connect :: Socket -> Address -> IO (Either Errno Endpoint)
connect (Socket fd) address =
  Address.asCString address $ \caddr -> do
    endpoint :: CInt <-
      nn_connect fd caddr

    if endpoint < 0
      then Left <$> getErrno
      else pure (Right (Endpoint endpoint))

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


--------------------------------------------------------------------------------
-- Socket options
--------------------------------------------------------------------------------

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

ipv4Only ::
     Socket
  -> IO (Either Errno Bool)
ipv4Only socket =
  (fmap.fmap)
    (\n -> if n == 0 then False else True)
    (getsockopt_int socket levelSocket optionIpv4only)

maxReconnectInterval ::
     Socket
  -> IO (Either Errno CInt)
maxReconnectInterval socket =
  getsockopt_int socket levelSocket optionReconnectIvlMax

maxRecvSize ::
     Socket
  -> IO (Either Errno CInt)
maxRecvSize socket =
  getsockopt_int socket levelSocket optionRcvmaxsize

maxTTL ::
     Socket
  -> IO (Either Errno CInt)
maxTTL socket =
  getsockopt_int socket levelSocket optionMaxttl

reconnectInterval ::
     Socket
  -> IO (Either Errno CInt)
reconnectInterval socket =
  getsockopt_int socket levelSocket optionReconnectIvl

recvBufferSize ::
     Socket
  -> IO (Either Errno CInt)
recvBufferSize socket =
  getsockopt_int socket levelSocket optionRcvbuf

recvFd ::
     Socket
  -> IO (Either Errno Fd)
recvFd socket =
  coerce (getsockopt_int socket levelSocket optionRcvfd)

recvPriority ::
     Socket
  -> IO (Either Errno CInt)
recvPriority socket =
  getsockopt_int socket levelSocket optionRcvprio

recvTimeout ::
     Socket
  -> IO (Either Errno CInt)
recvTimeout socket =
  getsockopt_int socket levelSocket optionRcvtimeo

sendBufferSize ::
     Socket
  -> IO (Either Errno CInt)
sendBufferSize socket =
  getsockopt_int socket levelSocket optionSndbuf

sendFd ::
     Socket
  -> IO (Either Errno Fd)
sendFd socket =
  coerce (getsockopt_int socket levelSocket optionSndfd)

sendPriority ::
     Socket
  -> IO (Either Errno CInt)
sendPriority socket =
  getsockopt_int socket levelSocket optionSndprio

sendTimeout ::
     Socket
  -> IO (Either Errno CInt)
sendTimeout socket =
  getsockopt_int socket levelSocket optionSndtimeo


--------------------------------------------------------------------------------
-- Messaging
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- Socket statistics
--------------------------------------------------------------------------------

getStatistic ::
     Socket
  -> CInt
  -> IO (Either Errno CULong)
getStatistic (Socket fd) stat = do
  value :: CULong <-
    nn_get_statistic fd stat

  if value == maxBound
    then Left <$> getErrno
    else pure (Right value)

acceptedConnections ::
     Socket
  -> IO (Either Errno CULong)
acceptedConnections socket =
  getStatistic socket (#const NN_STAT_ACCEPTED_CONNECTIONS)

acceptErrors ::
     Socket
  -> IO (Either Errno CULong)
acceptErrors socket =
  getStatistic socket (#const NN_STAT_ACCEPT_ERRORS)

bindErrors ::
     Socket
  -> IO (Either Errno CULong)
bindErrors socket =
  getStatistic socket (#const NN_STAT_BIND_ERRORS)

brokenConnections ::
     Socket
  -> IO (Either Errno CULong)
brokenConnections socket =
  getStatistic socket (#const NN_STAT_BROKEN_CONNECTIONS)

bytesReceived ::
     Socket
  -> IO (Either Errno CULong)
bytesReceived socket =
  getStatistic socket (#const NN_STAT_BYTES_RECEIVED)

bytesSent ::
     Socket
  -> IO (Either Errno CULong)
bytesSent socket =
  getStatistic socket (#const NN_STAT_BYTES_SENT)

connectErrors ::
     Socket
  -> IO (Either Errno CULong)
connectErrors socket =
  getStatistic socket (#const NN_STAT_CONNECT_ERRORS)

currentConnections ::
     Socket
  -> IO (Either Errno CULong)
currentConnections socket =
  getStatistic socket (#const NN_STAT_CURRENT_CONNECTIONS)

droppedConnections ::
     Socket
  -> IO (Either Errno CULong)
droppedConnections socket =
  getStatistic socket (#const NN_STAT_DROPPED_CONNECTIONS)

establishedConnections ::
     Socket
  -> IO (Either Errno CULong)
establishedConnections socket =
  getStatistic socket (#const NN_STAT_ESTABLISHED_CONNECTIONS)

messagesReceived ::
     Socket
  -> IO (Either Errno CULong)
messagesReceived socket =
  getStatistic socket (#const NN_STAT_MESSAGES_RECEIVED)

messagesSent ::
     Socket
  -> IO (Either Errno CULong)
messagesSent socket =
  getStatistic socket (#const NN_STAT_MESSAGES_SENT)

--------------------------------------------------------------------------------
-- Error reporting
--------------------------------------------------------------------------------

-- | <https://nanomsg.org/v1.1.5/nn_strerror.html>
strerror :: Errno -> IO Text
strerror (Errno errno) =
  Text.pack <$> (peekCString =<< nn_strerror errno)


--------------------------------------------------------------------------------
-- Teardown
--------------------------------------------------------------------------------

-- | <https://nanomsg.org/v1.1.5/nn_term.html>
term :: IO ()
term =
  nn_term


--------------------------------------------------------------------------------
-- Nanomsg version
--------------------------------------------------------------------------------

version :: (Int, Int)
version =
  (x, y)
  where
    x = #const NN_VERSION_CURRENT
    y = #const NN_VERSION_REVISION

-- Misc

device ::
     Socket
  -> Socket
  -> IO (Either Errno Void)
device (Socket fd1) (Socket fd2) =
  go

  where
    go :: IO (Either Errno Void)
    go = do
      _ <- nn_device fd1 fd2
      errno <- getErrno
      if errno == eINTR
        then go
        else pure (Left errno)
