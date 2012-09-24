{-# LANGUAGE OverloadedStrings #-}
-- | Module    : NLP.Antfarm.English
-- Copyright   : 2012 Eric Kow (Computational Linguistics Ltd.)
-- License     : BSD3
-- Maintainer  : eric.kow@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions to realise antfarm output in English.
--
-- We're not under any illusions that antfarm will work well for
-- languages other than English, but it could still be useful to
-- try anyway.
module NLP.Antfarm.English where

import Data.Text ( Text )
import Data.Tree ( Tree(..) )
import qualified Data.Text as T

import NLP.Minimorph.English
import NLP.Minimorph.Number
import NLP.Minimorph.Util
import NLP.Antfarm.Refex

-- | English realisation for a referring expression
englishRx :: [Tree SubRx] -> Text
englishRx =
    commas "and" . map (expand . fmap englishSubrx)
  where
    expand (Node main exs)  = main <+> examples exs
    examples [] =  ""
    examples xs = "(" <> T.intercalate ", " (map expand xs) <> ")"

-- | English realisation for a referring expression subunit (this can be useful
--   if you need special formatting between units, eg. bullet points)
englishSubrx :: SubRx -> Text
englishSubrx x =
    T.unwords $ discr ++ [body]
  where
    num   = srxNumber x
    discr = englishDiscriminator (srxDet x) (srxDiscriminator x)
    body  = fromSP num $ srxWord x

englishDiscriminator :: SingPlu [Text] -- ^ default determiner
                     -> Discriminator
                     -> [Text]
englishDiscriminator det discr =
    case discr of
        NilDiscriminator -> []
        TheSame          -> [ "the", "same"    ]
        TheOther         -> [ "the", "other"   ]
        TheOrdinal n     -> [ "the", ordinal n ]
        NewOrdinal 2     -> [ "another" ] -- prefer "another" to "a second"
        NewOrdinal n     -> [ indefiniteDet (ordinal n), ordinal n ]
        Another 1        -> [ "another" ]
        Another n        -> [ "another", cardinal n ]
        PlainCardinal 1  -> sg det
        PlainCardinal n  -> [ cardinal n ]
        CardinalOfThe n  -> [ cardinal n, "of", "the" ]
        The              -> [ "the" ]
        Bounded (SayAtLeast n)   -> [ "at", "least", tnum n ]
        Bounded (SayAtMost  n)   -> [ "at", "most" , tnum n ]
        Bounded (SayExactly n)   -> [ "exactly", tnum n ]
        Bounded (SayBetween n m) -> [ "between", tnum n, "and", tnum m ]
        Bounded (SayArbitrary t) -> [ t ]
  where
    tnum = T.pack . show

-- defaultDet :: Text -> SingPlu [Text]
-- defaultDet word = SP [indefiniteDet word] ["the"]
