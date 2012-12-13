{-# LANGUAGE OverloadedStrings #-}
-- | Module    : NLP.Antfarm.Spanish
-- Copyright   : 2012 Eric Kow (Computational Linguistics Ltd.)
-- License     : BSD3
-- Maintainer  : eric.kow@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions to realise antfarm output in Spanish.
--
-- This code won't work well until Minimorph Spanish is ready.
-- Buena suerte.

module NLP.Antfarm.Spanish where

import Data.Text ( Text )
import Data.Tree ( Tree(..) )
import qualified Data.Text as T

import NLP.Minimorph.English -- it should be 'Spanish'
import NLP.Minimorph.Number
import NLP.Minimorph.Util
import NLP.Antfarm.Refex

-- | Spanish realisation for a referring expression
spanishRx :: [Tree SubRx] -> Text
spanishRx =
    commas "y" . map (expand . fmap spanishSubrx)
  where
    expand (Node main exs)  = main <+> examples exs
    examples [] =  ""
    examples xs = "(" <> T.intercalate ", " (map expand xs) <> ")"

-- | Spanish realisation for a referring expression subunit (this can be useful
--   if you need special formatting between units, eg. bullet points)
spanishSubrx :: SubRx -> Text
spanishSubrx x =
    T.unwords $ discr ++ [body]
  where
    num   = srxNumber x
    discr = spanishDiscriminator (srxDet x) (srxDiscriminator x)
    body  = fromSP num $ srxWord x

spanishDiscriminator :: SingPlu [Text] -- ^ default determiner
                     -> Discriminator
                     -> [Text]
spanishDiscriminator det discr =
    case discr of
        NilDiscriminator -> []
        TheSame          -> [ "el", "mismo" ] -- need gender information and article morphology here
        TheOther         -> [ "el", "otro" ]
        TheOrdinal n     -> [ "el", ordinal n ]
        NewOrdinal 2     -> [ "otro" ] -- prefer "another" to "a second"
        NewOrdinal n     -> [ indefiniteDet (ordinal n), ordinal n ]
        Another 1        -> [ "otro" ]
        Another n        -> [ "otro", cardinal n ]
        PlainCardinal 1  -> sg det
        PlainCardinal n  -> [ cardinal n ]
        CardinalOfThe n  -> [ cardinal n, "de", "los" ]
        The              -> [ "el" ]
        Bounded (SayAtLeast n)   -> [ "por", "lo", "menos", cardinal n ]
        Bounded (SayAtMost  n)   -> [ "como", "máximo" , cardinal n ]
        Bounded (SayExactly n)   -> [ "justo", cardinal n ]
        Bounded (SayBetween n m) -> [ "entre", cardinal n, "y", cardinal m ]
        Bounded (SayArbitrary t) -> [ t ]
