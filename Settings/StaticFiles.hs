{-# LANGUAGE TemplateHaskell #-}
module Settings.StaticFiles where

import           Data.Default                    (def)
import           Filesystem.Path                 ()
import           Language.Haskell.TH             (Exp, Name, Q)
import           Prelude                         (IO)
import           Settings                        (staticDir)
import           Settings.Development
import           Yesod.EmbeddedStatic.Generators
import           Yesod.Static
import qualified Yesod.Static                    as Static


-- | use this to create your static file serving site
staticSite :: IO Static.Static
staticSite = if development then Static.staticDevel staticDir
                            else Static.static      staticDir

-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
--   Warning: any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
$(staticFiles Settings.staticDir)

combineSettings :: CombineSettings
combineSettings =
  def { csJsPostProcess = \_ -> closureJs
      , csCssPostProcess = \_ -> yuiCSS }


-- csJsPostProcess settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets' development combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts' development combineSettings


