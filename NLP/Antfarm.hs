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


-- Referring expression generation:
--
-- 1. convert from abstract input to a referring expression
--    (NLP.Antfarm.Refex) with the help of a discourse history
--    (NLP.Antfarm.History)
-- 2. render into English (NLP.Antfarm.English)
-- 3. update the discourse history
--
-- The main focus is on #1 and #3, with #2 tending to be more
-- superficial and also application-specific (your application
-- may want to render the refexes in different ways)
import NLP.Antfarm.Refex
import NLP.Antfarm.Internal
import NLP.Antfarm.History
import NLP.Antfarm.English
