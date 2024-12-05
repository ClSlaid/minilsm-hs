module Lsm.Error (LsmError(..), LsmErrorType(..), LsmResult) where

import System.IO.Error qualified as SIE

newtype LsmError = LsmError
    { lsmErrType :: LsmErrorType
    }
    deriving (Show)

data LsmErrorType
    = IoErr
        { ioErr :: SIE.IOError
        , ioErrInfo :: String
        }
    | Other {otherErr :: String}
    deriving (Show)

type LsmResult a = Either LsmError a
