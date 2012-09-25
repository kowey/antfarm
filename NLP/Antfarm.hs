{-# LANGUAGE DeriveDataTypeable, ViewPatterns, OverloadedStrings, TupleSections #-}

-- |
-- Copyright   : 2012 Eric Kow (Computational Linguistics Ltd.)
-- License     : BSD3
-- Maintainer  : eric.kow@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Referring expression generation for definitions.
--
--
module NLP.Antfarm
    (
    -- * Key structures
      module NLP.Antfarm.Refex
    -- * Discourse history
    , RefHistory(..)
    , addToHistory, emptyHistory, noteImplicitBounds
    -- * Core generation
    , rx, subrx, subrxNumber
    -- * English surface form
    , englishRx, englishSubrx
    )
  where

import NLP.Antfarm.Internal
import NLP.Antfarm.Refex
import NLP.Antfarm.English
import NLP.Antfarm.History
