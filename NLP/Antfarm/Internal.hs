{-# LANGUAGE DeriveDataTypeable, ViewPatterns, OverloadedStrings, TupleSections #-}

-- |
-- Copyright   : 2012 Eric Kow (Computational Linguistics Ltd.)
-- License     : BSD3
-- Maintainer  : eric.kow@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- The heart of the referring expression generation (see 'rx')
module NLP.Antfarm.Internal where

import Data.Maybe ( fromMaybe )
import Data.Tree ( Tree(..) )
import Prelude hiding ( lex )
import qualified Data.Set  as Set

import NLP.Minimorph.Number

import NLP.Antfarm.Refex
import NLP.Antfarm.History

-- ----------------------------------------------------------------------
-- concepts (sub-unit of referring expressions)
-- ----------------------------------------------------------------------
-- englishDiscriminator (fromSP agr (srxDet srx))
-- word  = fromSP agr (srxWord srx)

-- | Decide how to realise a referring expression
rx :: RefHistory        -- ^ prior decisions
   -> [Tree SubRxInput] -- ^ expression in question (is tree shaped to
                        --   allow for examples; see note 'discourse tree')
   -> [Tree SubRx]      -- ^ one abstract decision for each piece of the
                        --   input
rx st = map (subrx st)

-- | Decide how to realise a single unit within a referring expression
--   The main decisions we make are on the number which we should associate
--   with the noun (singular vs plural), and what sort of "discriminator"
--   expression we could use with it ("the", vs "another", vs "3" etc)
--
--   Keep in mind that this is only for one 'DiscourseUnit' within a single rx.
--   An rx may involve multiple discourse units (eg. 3 cats and 1 dog)
subrx :: RefHistory       -- ^ prior decisions
      -> Tree SubRxInput  -- ^ expressions to realise
      -> Tree SubRx       -- ^ abstract decision per piece of input
subrx st (Node srx egs) =
    (Node root kids)
  where
    root  = SubRx num discr (srxInpDet srx) (srxInpWord srx)
    num   = surfaceNumber st du
    discr = discriminate  st du
    du    = srxInpEntity srx
    -- examples
    kids | null egs               = []
         | lastMentions st du > 0 = []
         | otherwise              = map (subrx st) egs

-- ----------------------------------------------------------------------
-- Number
-- ----------------------------------------------------------------------

-- | Helper for 'rx'
--
--   Whether the noun in a 'DiscourseUnit' should be realised as singular
--   or plural (note: the surface number may not actually correspond to
--   the actual number; see 'subrxNumber' for details)
surfaceNumber :: RefHistory -> DiscourseUnit -> Number
surfaceNumber st du =
    case subrxNumber du of
        FN_Singular      -> Singular
        FN_Plural        -> Plural
        FN_MaybeSingular -> if override then Plural else Singular
  where
    -- realise as plural if we are pointing back to something else
    -- we had already realised as part of a group
    -- [some dogs] ... one of the dog*s*
    override = hasTidyBackpointer st du

-- | Whether a 'DiscourseUnit' should be considered *morally*
--   (semantically) singular or plural.  The actual form used may be
--   different (see 'conceptNumber' because of deeper issues that
--   override this).
--
--   Consider one of the *dogs*; here the rx number is 'Singular'
--   — one dog — but on the surface we use the 'Plural' (the NP
--   'the dogs' is itself plural).  This discrepency is partly due
--   to the hacky way we've written this.  A cleaner implementation
--   would recursively realise 'the dogs' as a separate expression
--   with its own number.
subrxNumber :: DiscourseUnit -> FuzzyNumber
subrxNumber (Node rg _) =
    fromMaybe usualAgr (constrAgr bounds)
  where
    bounds  = rgBounds rg
    count   = Set.size (rgIdxes rg)
    -- default agreement; to be used if there are no constraints on
    -- agreement to be found
    usualAgr
        | isClasswide rg  = FN_Plural -- dog*s* (in general) like food
        | count == 1      = FN_MaybeSingular -- special case on 1
        | otherwise       = FN_Plural -- default to plural
    -- contraints on agreement that may arise from lower/upper bound
    -- constraints
    -- (TODO not sure if this really belongs here or more in the surface?)
    constrAgr (Bounds [] l u) =
        case (l,u) of
           -- at least, exactly, at most 1 => singular
           (Just 1, Nothing)  -> Just FN_Singular -- TODO hmm?
           (Just 1, Just 1)   -> Just FN_Singular
           (Nothing, Just 1)  -> Just FN_Singular -- TODO hmm?
           -- no constraints at all; we got nothing
           (Nothing, Nothing) -> Nothing
           -- any other type of constraint (eg. at least 2,
           -- at most 5, between 3 and 7); treat as plural
           _                  -> Just FN_Plural
    -- fallback case if there are malformed constraints
    constrAgr (Bounds _ _ _) = Just FN_Plural

-- ----------------------------------------------------------------------
-- Discriminator
-- ----------------------------------------------------------------------

-- | Helper for 'rx'
--
--   A discriminator is what we call the optional bit of text that helps
--   you distinguish one set instances of a class from another, eg,
--   “the same” or “another three”, or simply “the“
discriminate :: RefHistory   -- ^ discourse history
             -> DiscourseUnit
             -> Discriminator
discriminate st du@(Node rg _) =
    helper $ boundsText bounds
  where
    keys    = Set.toList (refKeys rg)
    bounds  = rgBounds rg
    count   = Set.size (rgIdxes rg)
    -- bounds take priority over other decisions
    helper (Just bnds) = case keys of
        _ | lastMentions st du > 0             -> TheSame
          | otherwise                          -> Bounded bnds
    -- these decisions aren't neccesarily principled, just a
    -- rough priority order based on what we think would be appropriate
    helper Nothing = case keys of
        [k] | isTheOther st k                  -> TheOther
        [mentionOrder st -> Just i]            -> TheOrdinal i -- the 7th ant
        [distractorGroups st -> ds@(_:_)]      -> NewOrdinal (1 + length ds) -- an 8th ant
        _   | isClasswide rg                   -> NilDiscriminator -- ants
            | any (hasDistractorGroup st) keys -> Another count -- another 2 ants
            | all (isFirstMention st)     keys -> PlainCardinal count -- 3 ants
            | hasTidyBackpointer st du         -> CardinalOfThe count -- 3 of the ants
            | lastMentions st du > 0           -> The -- the ant/ants
            | otherwise                        -> PlainCardinal count -- 3 ants

-- | If there are any unknown constraints, we pick the first one.
--   Otherwise, we generate an expression appropriate for the lower/upper bounds
boundsText :: Bounds -> Maybe BoundsExpr
boundsText (Bounds [] l u)    =
    case (l,u) of
        (Nothing, Nothing)   -> Nothing
        (Nothing, Just x)    -> Just $ SayAtMost  x
        (Just x, Nothing)    -> Just $ SayAtLeast x
        (Just x1, Just x2)
           | x1 == x2        -> Just $ SayExactly x1
           | otherwise       -> Just $ SayBetween x1 x2
boundsText (Bounds (x:_) _ _) = Just $ SayArbitrary x
