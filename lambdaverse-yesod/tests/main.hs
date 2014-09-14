{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Application          (makeFoundation)
import           HexagonTests
import           HomeTest
import           Import
import           Test.Hspec           (describe, hspec)
import           Yesod.Default.Config
import           Yesod.Test

main :: IO ()
main = do
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing)
                { csParseExtra = parseExtra
                }
    foundation <- makeFoundation conf
    hspec $ do
      describe "testing lambadaverse" $ do
        hexagonSpec
        yesodSpec foundation $ do
                homeSpecs
