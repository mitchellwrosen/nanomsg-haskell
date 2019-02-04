module Libnanomsg.Transport
  ( Transport(..)
  , transportInproc
  , transportIpc
  , transportTcp
  , transportWs
  ) where

import Data.Text (Text)

newtype Transport
  = Transport { unTransport :: Text }

transportInproc :: Transport
transportInproc =
  Transport "inproc://"

transportIpc :: Transport
transportIpc =
  Transport "ipc://"

transportTcp :: Transport
transportTcp =
  Transport "tcp://"

transportWs :: Transport
transportWs =
  Transport "ws://"
