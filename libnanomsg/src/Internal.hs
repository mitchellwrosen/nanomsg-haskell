module Internal where

import Data.Kind (Type)

data Error :: Operation -> Type where
  EADDRINUSE      :: (MayReturnEADDRINUSE      op ~ 'True) => Error op
  EADDRNOTAVAIL   :: (MayReturnEADDRNOTAVAIL   op ~ 'True) => Error op
  EAFNOSUPPORT    :: (MayReturnEAFNOSUPPORT    op ~ 'True) => Error op
  EBADF           :: (MayReturnEBADF           op ~ 'True) => Error op
  EINVAL          :: (MayReturnEINVAL          op ~ 'True) => Error op
  EMFILE          :: (MayReturnEMFILE          op ~ 'True) => Error op
  ENAMETOOLONG    :: (MayReturnENAMETOOLONG    op ~ 'True) => Error op
  ENODEV          :: (MayReturnENODEV          op ~ 'True) => Error op
  ENOPROTOOPT     :: (MayReturnENOPROTOOPT     op ~ 'True) => Error op
  EPROTONOSUPPORT :: (MayReturnEPROTONOSUPPORT op ~ 'True) => Error op
  ETERM           :: (MayReturnETERM           op ~ 'True) => Error op

data Operation
  = NnBind
  | NnClose
  | NnConnect
  | NnGetsockopt
  | NnSetsockopt
  | NnSocket

type family MayReturnEADDRINUSE (op :: Operation) :: Bool where
  MayReturnEADDRINUSE 'NnBind = 'True
  MayReturnEADDRINUSE _       = 'False

type family MayReturnEADDRNOTAVAIL (op :: Operation) :: Bool where
  MayReturnEADDRNOTAVAIL 'NnBind = 'True
  MayReturnEADDRNOTAVAIL _       = 'False

type family MayReturnEAFNOSUPPORT (op :: Operation) :: Bool where
  MayReturnEAFNOSUPPORT 'NnSocket = 'True
  MayReturnEAFNOSUPPORT _         = 'False

type family MayReturnEBADF (op :: Operation) :: Bool where
  MayReturnEBADF 'NnBind       = 'True
  MayReturnEBADF 'NnClose      = 'True
  MayReturnEBADF 'NnConnect    = 'True
  MayReturnEBADF 'NnGetsockopt = 'True
  MayReturnEBADF 'NnSetsockopt = 'True
  MayReturnEBADF _             = 'False

type family MayReturnEINVAL (op :: Operation) :: Bool where
  MayReturnEINVAL 'NnBind       = 'True
  MayReturnEINVAL 'NnConnect    = 'True
  MayReturnEINVAL 'NnSetsockopt = 'True
  MayReturnEINVAL 'NnSocket     = 'True
  MayReturnEINVAL _             = 'False

type family MayReturnEMFILE (op :: Operation) :: Bool where
  MayReturnEMFILE 'NnBind    = 'True
  MayReturnEMFILE 'NnConnect = 'True
  MayReturnEMFILE 'NnSocket  = 'True
  MayReturnEMFILE _          = 'False

type family MayReturnENAMETOOLONG (op :: Operation) :: Bool where
  MayReturnENAMETOOLONG 'NnBind    = 'True
  MayReturnENAMETOOLONG 'NnConnect = 'True
  MayReturnENAMETOOLONG _          = 'False

type family MayReturnENODEV (op :: Operation) :: Bool where
  MayReturnENODEV 'NnBind    = 'True
  MayReturnENODEV 'NnConnect = 'True
  MayReturnENODEV _          = 'False

type family MayReturnENOPROTOOPT (op :: Operation) :: Bool where
  MayReturnENOPROTOOPT 'NnGetsockopt = 'True
  MayReturnENOPROTOOPT 'NnSetsockopt = 'True
  MayReturnENOPROTOOPT _             = 'False

type family MayReturnEPROTONOSUPPORT (op :: Operation) :: Bool where
  MayReturnEPROTONOSUPPORT 'NnConnect = 'True
  MayReturnEPROTONOSUPPORT 'NnBind    = 'True
  MayReturnEPROTONOSUPPORT _          = 'False

type family MayReturnETERM (op :: Operation) :: Bool where
  MayReturnETERM 'NnBind       = 'True
  MayReturnETERM 'NnConnect    = 'True
  MayReturnETERM 'NnGetsockopt = 'True
  MayReturnETERM 'NnSetsockopt = 'True
  MayReturnETERM 'NnSocket     = 'True
  MayReturnETERM _             = 'False
