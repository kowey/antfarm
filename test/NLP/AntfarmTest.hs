{-# LANGUAGE OverloadedStrings #-}

module NLP.AntfarmTest where

{-
import Control.Applicative
import Control.Monad ( when )
import Data.Char ( isPunctuation, isAlpha )
import Data.List
import Data.String ( IsString(..) )
import Data.Tree
import qualified Data.Map  as Map

import Test.QuickCheck
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework
import Text.HTML.TagSoup

import NLP.Aura.AuraMorph
import NLP.Aura.AuraLex
import NLP.Aura.Cardinality
import NLP.Aura.MResult
import NLP.Aura.MResult
import Lexicon
import RefTest
import TestUtil
-}

import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.State
import Data.Text ( Text )
import qualified Data.Text as T

import NLP.Minimorph.Util
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework

import NLP.Antfarm
import NLP.Antfarm.Internal
import NLP.Antfarm.Demo
import NLP.Antfarm.Refex

suite :: Test.Framework.Test
suite = testGroup "NLP.Antfarm"
    [ t_rx
    , t_rx_es  
    ]

data RefTest = RT
    { rtOut :: Text
    , rtInp :: Text
    }

t_rx :: Test.Framework.Test
t_rx = testGroup "rx"
    -- -------------------------------------------------- within
    [ testGroup "within rx"
        [ tcase [ RT "a box"                "b1" ]
        , tcase [ RT "a box and an ant"     "b1 a1" ]
        , tcase [ RT "two boxes"            "b1 b2" ]
        , tcase [ RT "two boxes and an ant" "b1 a1 b2" ]
        , tcase [ RT "an ant and two boxes" "a1 b1 b2" ]
        , tcase [ RT "at least one ant"       "a>=1 a"  ]
        , tcase [ RT "at least two ants"      "a>=2 a0" ]
        , tcase [ RT "at least two ants"      "a>=2"    ]
        , tcase [ RT "at least two ants"      "a>=2 a"  ]
        , tcase [ RT "at least three ants"      "a>=3 a>=2 a" ]
        , tcase [ RT "at least three ants"      "a>=2 a>=3 a" ]
        , tcase [ RT "exactly one ant"        "a=1  a0" ]
        , tcase [ RT "exactly one ant"        "a=1  a"  ]
        , tcase [ RT "exactly three ants"     "a=3  a"  ]
        , tcase [ RT "between two and three ants" "a>=2 a<=3 a" ]
        , tcase [ RT "between two and three ants" "a>=2 a<=3 a" ]
        , tcase [ RT "between one and three ants" "a<=3 a1" ]
        , tcase [ RT "between two and three ants" "a<=3 a1 a2" ]
        , tcase [ RT "two ants (exactly three boxes, a cat)"
                  "a1 a2 (b=3 c1)" ]
        , tcase [ RT "two ants (exactly three boxes (two dogs), a cat)"
                  "a1 a2 (b=3 (d1 d3) c1)" ]
        ]
    -- -------------------------------------------------- across
    , testGroup "across rx"
        [ tcase [ RT "an ant"          "a1"
                , RT "the ant"         "a1"
                ] 
        , tcase [ RT "an ant"          "a1"
                , RT "the ant"         "a1"
                , RT "the ant"         "a1" -- ords should not kick in
                ]
        , tcase [ RT "an ant"          "a1"
                , RT "another ant"     "a2"
                ]
        , tcase [ RT "an ant"          "a1"
                , RT "another ant"     "a2"
                , RT "the first ant"   "a1"
                ]
        , tcase [ RT "an ant"          "a1"
                , RT "another ant"     "a2"
                , RT "the second ant"  "a2"
                ]
        , tcase [ RT "an ant"          "a1"
                , RT "another ant"     "a2"
                , RT "a third ant"     "a3"
                , RT "the second ant"  "a2"
                ] 
        , tcase [ RT "two ants"        "a1 a2"
                , RT "one of the ants" "a1"
                ]
        , tcase [ RT "two ants"        "a1 a2"
                , RT "one of the ants" "a2"    -- order don't matter
                ]
        , tcase [ RT "two ants"        "a1 a2"
                , RT "the ants"        "a2 a1"
                ]
        , tcase [ RT "at least three ants" "a>=3 a1"
                , RT "at least three ants" "a>=3 a2" -- a1 != a2
                ]
        , tcase [ RT "at least three ants" "a>=3 a1"
                , RT "at least two ants"   "a>=2 a1" -- 2 != 3
                ]
        , tcase [ RT "at least three ants" "a>=3 a1"
                , RT "the same ants"       "a>=3 a1"
                ]
        , tcase [ RT "two ants"            "a1 a2"
                , RT "one of the ants and at most three boxes" "a1 b<=3"
                ]
        , tcase [ RT "two ants"            "a1 a2"
                , RT "one of the ants and between one and three boxes" "a1 b<=3 b2"
                ]
        , tcase [ RT "two ants"        "a1 a2"
                , RT "one of the ants" "a2"
                , RT "the other ant"   "a1"
                ]
        , tcase [ RT "ants"            "a"
                , RT "an ant"          "a2"
                , RT "ants"            "a"
                , RT "another ant"     "a1"
                ]
        , tcase [ RT "at least two animals (a cat)"     "A>=2 (c1)"
                , RT "the same animals"                 "A>=2 (c1)"
                ]
        , tcase [ RT "at least two animals (a cat)"        "A>=2 (c1)"
                , RT "at least two animals (another cat)"  "A>=2 (c2)"
                ]
      ] -- a third ant
        -- , RT "the first ant"   "a2"
        -- , RT "the second ant"  "a1"
    ]
  where
    descr xs = T.unpack $
        intercalateRx (map rtInp xs)
        <+> (parens . intercalateRx $ map rtOut xs)
    tcase xs = testCase (descr xs) $ assertRefTests englishNextRx xs
    
t_rx_es :: Test.Framework.Test
t_rx_es = testGroup "rx_es"
    -- -------------------------------------------------- within
    [ testGroup "simple Spanish"
        [ tcase [ RT "a sombrero"                "S1"  -- wrong, si si
                , RT "el sombrero"               "S1"  
                , RT "otro sombrero"             "S2"  
                ]
        ]
    ]
  where
    descr xs = T.unpack $
        intercalateRx (map rtInp xs)
        <+> (parens . intercalateRx $ map rtOut xs)
    tcase xs = testCase (descr xs) $ assertRefTests spanishNextRx xs


assertRefTests :: ([DiscourseUnit] -> RefStateT IO Text) -> [RefTest] -> IO ()
assertRefTests languageRx rts =
    evalStateT (mapM_ next rts) emptyHistory
  where
    next rt =
       case decodeRx (T.unpack (rtInp rt)) of
          Left err  -> liftIO $ assertFailure (show err)
          Right rxs -> do
              res <- languageRx rxs
              let msg = T.unpack (rtOut rt)
              liftIO $ assertEqual msg (rtOut rt) res
