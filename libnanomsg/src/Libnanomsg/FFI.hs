module Libnanomsg.FFI where

import Foreign.C
import Foreign.Ptr (Ptr)
import GHC.Prim (Addr#)

foreign import ccall safe "nn_bind"
  nn_bind :: CInt -> CString -> IO CInt

foreign import ccall safe "nn_close"
  nn_close :: CInt -> IO CInt

foreign import ccall safe "nn_connect"
  nn_connect :: CInt -> CString -> IO CInt

foreign import ccall safe "nn_getsockopt"
  nn_getsockopt :: CInt -> CInt -> CInt -> Ptr a -> Ptr CSize -> IO CInt

foreign import ccall safe "nn_recv"
  nn_recv :: CInt -> Addr# -> CSize -> CInt -> IO CInt

foreign import ccall safe "nn_send"
  nn_send :: CInt -> Addr# -> CSize -> CInt -> IO CInt

foreign import ccall safe "nn_setsockopt"
  nn_setsockopt :: CInt -> CInt -> CInt -> Ptr a -> CSize -> IO CInt

foreign import ccall safe "nn_shutdown"
  nn_shutdown :: CInt -> CInt -> IO CInt

foreign import ccall safe "nn_socket"
  nn_socket :: CInt -> CInt -> IO CInt

foreign import ccall safe "nn.h nn_strerror"
  nn_strerror :: CInt -> IO CString

foreign import ccall safe "nn.h nn_term"
  nn_term :: IO ()
