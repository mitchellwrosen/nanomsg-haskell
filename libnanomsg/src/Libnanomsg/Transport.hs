module Libnanomsg.Transport
  ( Transport(..)
  , toText
  ) where

import Data.Text (Text)

data Transport
  = Inproc
  | Ipc
  | Tcp
  | Ws

toText :: Transport -> Text
toText = \case
  Inproc -> "inproc://"
  Ipc -> "ipc://"
  Tcp -> "tcp://"
  Ws -> "ws://"
