module Internal where

import Data.Kind (Type)

data Error :: Operation -> Type where
  AddressFamilyNotSupported ::
       (MayReturnAddressFamilyNotSupported op ~ 'True)
    => Error op

  Terminating ::
       (MayReturnTerminating op ~ 'True)
    => Error op

  TooManyOpenFiles ::
       (MayReturnTooManyOpenFiles op ~ 'True)
    => Error op

  UnknownProtocol ::
       (MayReturnUnknownProtocol op ~ 'True)
    => Error op

data Operation
  = NnSocket

type family MayReturnAddressFamilyNotSupported (op :: Operation) :: Bool where
  MayReturnAddressFamilyNotSupported 'NnSocket = 'True

type family MayReturnTerminating (op :: Operation) :: Bool where
  MayReturnTerminating 'NnSocket = 'True

type family MayReturnTooManyOpenFiles (op :: Operation) :: Bool where
  MayReturnTooManyOpenFiles 'NnSocket = 'True

type family MayReturnUnknownProtocol (op :: Operation) :: Bool where
  MayReturnUnknownProtocol 'NnSocket = 'True
