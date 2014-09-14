{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Handler.Home where

import           Import
import           Language.Haskell.TH

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        let application_name = "sector map"
        setTitle application_name
        $(widgetFile "homepage")
        $(fayFile' (ConE 'StaticR) "Home")

getHexagonTestR :: Handler Html
getHexagonTestR = do
    defaultLayout $ do
      let application_name = "hexagon self test"
      setTitle application_name
      addScriptRemote "//code.jquery.com/qunit/qunit-1.15.0.js"
      addStylesheetRemote "//code.jquery.com/qunit/qunit-1.15.0.css"
      $(widgetFile "hexagon_selftest")
      $(fayFile' (ConE 'StaticR) "HexagonSelftest")
