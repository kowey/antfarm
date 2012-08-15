{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import System.Exit

import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework

import qualified NLP.AntfarmTest

main :: IO ()
main = defaultMain
    [ NLP.AntfarmTest.suite
    ]
