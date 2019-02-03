module Internal where

import Data.Kind (Type)

data Error :: Operation -> Type where
  AddressFamilyNotSupported ::
       (MayReturnAddressFamilyNotSupported op ~ 'True)
    => Error op

  InvalidOption ::
       (MayReturnInvalidOption op ~ 'True)
    => Error op

  InvalidProtocol ::
       (MayReturnInvalidProtocol op ~ 'True)
    => Error op

  InvalidSocket ::
       (MayReturnInvalidSocket op ~ 'True)
    => Error op

  Terminating ::
       (MayReturnTerminating op ~ 'True)
    => Error op

  TooManyOpenFiles ::
       (MayReturnTooManyOpenFiles op ~ 'True)
    => Error op

data Operation
  = NnClose
  | NnSetsockopt
  | NnSocket

type family MayReturnAddressFamilyNotSupported (op :: Operation) :: Bool where
  MayReturnAddressFamilyNotSupported 'NnSocket = 'True
  MayReturnAddressFamilyNotSupported _ = 'False

type family MayReturnInvalidOption (op :: Operation) :: Bool where
  MayReturnInvalidOption 'NnSetsockopt = 'True
  MayReturnInvalidOption _ = 'False

type family MayReturnInvalidProtocol (op :: Operation) :: Bool where
  MayReturnInvalidProtocol 'NnSetsockopt = 'True
  MayReturnInvalidProtocol 'NnSocket = 'True
  MayReturnInvalidProtocol _ = 'False

type family MayReturnInvalidSocket (op :: Operation) :: Bool where
  MayReturnInvalidSocket 'NnClose = 'True
  MayReturnInvalidSocket 'NnSetsockopt = 'True
  MayReturnInvalidSocket _ = 'False

type family MayReturnTerminating (op :: Operation) :: Bool where
  MayReturnTerminating 'NnSetsockopt = 'True
  MayReturnTerminating 'NnSocket = 'True
  MayReturnTerminating _ = 'False

type family MayReturnTooManyOpenFiles (op :: Operation) :: Bool where
  MayReturnTooManyOpenFiles 'NnSocket = 'True
  MayReturnTooManyOpenFiles _ = 'False
