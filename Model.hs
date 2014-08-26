{-# LANGUAGE TemplateHaskell #-}

module Model where

import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           Database.Persist.MongoDB   hiding (master)
import           Database.Persist.Quasi
import           Language.Haskell.TH.Syntax
import           Prelude
import           Yesod

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
let mongoSettings = (mkPersistSettings (ConT ''MongoBackend))
                        { mpsGeneric = False
                        }
 in share [mkPersist mongoSettings]
    $(persistFileWith upperCaseSettings "config/models")
