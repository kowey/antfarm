{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- | Module    : NLP.Antfarm.Cardinality
-- Copyright   : 2012 Eric Kow (Computational Linguistics Ltd.)
-- License     : BSD3
-- Maintainer  : eric.kow@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Cardinality constraints
module NLP.Antfarm.Cardinality where

import Data.Text ( Text )

data Constraint = AtMost  Int
                | Exactly Int
                | AtLeast Int
                | Unknown Text
  deriving (Show, Eq, Ord)

lowerBound :: Constraint -> Maybe Int
lowerBound (AtLeast x) = Just x
lowerBound (Exactly x) = Just x
lowerBound (AtMost _)  = Nothing
lowerBound (Unknown _) = Nothing

upperBound :: Constraint -> Maybe Int
upperBound (AtLeast _) = Nothing
upperBound (Exactly x) = Just x
upperBound (AtMost x)  = Just x
upperBound (Unknown _) = Nothing

unknown :: Constraint -> Maybe Text
unknown (Unknown t) = Just t
unknown _           = Nothing
