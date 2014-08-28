{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}


module SharedTypes where


import           Prelude
#ifdef FAY
import           FFI
#els
import           Fay.FFI            ()
#endif

import           Data.Data
import           Language.Fay.Yesod


data FayCommand = FayCommand (Returns Text)
 deriving (Show, Read, Eq, Typeable, Data)


