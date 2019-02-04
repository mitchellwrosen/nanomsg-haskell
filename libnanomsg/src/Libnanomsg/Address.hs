module Libnanomsg.Address
  ( Address(..)
  , asCString
  ) where

import Data.ByteString (ByteString)
import Foreign.C

import qualified Data.ByteString as ByteString

data Address
  = Inproc ByteString
  | Ipc ByteString
  | Tcp ByteString
  | Ws ByteString

asCString :: Address -> (CString -> IO a) -> IO a
asCString address =
  ByteString.useAsCString (toByteString address)

toByteString :: Address -> ByteString
toByteString = \case
  Inproc addr -> "inproc://" <> addr
  Ipc    addr -> "ipc://"    <> addr
  Tcp    addr -> "tcp://"    <> addr
  Ws     addr -> "ws://"     <> addr
