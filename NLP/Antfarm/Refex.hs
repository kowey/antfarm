-- |
-- Copyright   : 2012 Eric Kow (Computational Linguistics Ltd.)
-- License     : BSD3
-- Maintainer  : eric.kow@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Referring expressions and discourse units
module NLP.Antfarm.Refex where

import Data.Maybe ( mapMaybe )
import Data.Text ( Text )
import Data.Tree ( Tree )
import qualified Data.Set as Set

import NLP.Minimorph.Number

import NLP.Antfarm.Cardinality

-- ----------------------------------------------------------------------
-- * Referring expressions
-- ----------------------------------------------------------------------

-- | Input needed to realise a subunit of a referring expression
--   A subunit corresponds to 'RefGroup' (but in practice,
--   we need the whole 'DiscourseUnit', not just the 'RefGroup'
--   root)
data SubRxInput = SubRxInput
    { srxInpDet    :: SingPlu [Text] -- ^ determiner (can be empty)
    , srxInpWord   :: SingPlu Text   -- ^ main word
    , srxInpEntity :: DiscourseUnit
    }

-- | Output for a subunit of a referring expression
data SubRx = SubRx
    { srxNumber        :: Number
    , srxDiscriminator :: Discriminator
    , srxDet           :: SingPlu [Text]
    , srxWord          :: SingPlu Text
    }
  deriving (Eq, Show)

-- | A single referring expression has subunits, each of which
--   potentially having examples
type RxInput = [Tree SubRxInput]

-- | A referring expression
type Rx = [Tree SubRx]

-- ----------------------------------------------------------------------
-- * Discourse units
-- ----------------------------------------------------------------------

-- | A discourse unit includes all instances and constraints needed
--   to uniquely identify it. (see note discourse tree)
--
--   In the current implementation, a referring expression may contain
--   more than one discourse unit.  So in a referring expression “three cats
--   and at most two dogs (a poodle and a labrador)”, the “at most two dogs
--   (a poodle and a labrador)” and “three cats” would each correspond to
--   different 'DiscourseUnit's
type DiscourseUnit = Tree RefGroup

-- | A sub-unit in a referring expression, instances of and/or constraints
--   over class.  So in a referring expression “three cats and at most two
--   dogs”, the “at most two dogs” and “three cats” would each be 'RefGroup's
data RefGroup = RefGroup
    { rgClass  :: Text
    , rgIdxes  :: Set.Set Text
    , rgBounds :: Bounds
    }
  deriving (Ord, Eq)

-- | Set of unique keys in a ref group
refKeys :: RefGroup -> Set.Set RefKey
refKeys rg = Set.map (\i -> (rgClass rg, i)) (rgIdxes rg)

-- | A unique object
type RefKey      = (Text, Text)

-- ----------------------------------------------------------------------
-- * Bounds
-- ----------------------------------------------------------------------

data Bounds = Bounds
    { bUnknown :: [Text]
        --  the whole idea of unknown constraints makes me very
        --  unhappy; we want to pass through any malformed
        --  constraints as is, so for now the only thing I can
        --  think to do with them is collect them in the groups
        --  which just seems wrong
    , bLower   :: Maybe Int -- ^ lower
    , bUpper   :: Maybe Int -- ^ upper
    }
  deriving (Ord, Eq)

emptyBounds :: Bounds
emptyBounds = Bounds [] Nothing Nothing

explicitBounds :: [Constraint] -> Bounds
explicitBounds cs = Bounds
    { bUnknown = [ t | Unknown t <- cs ]
    , bLower   = maximum `orNothing` mapMaybe lowerBound cs
    , bUpper   = minimum `orNothing` mapMaybe upperBound cs
    }
  where
    orNothing _ [] = Nothing
    orNothing f xs = Just (f xs)

-- | When two 'Bounds' are combined the result is narrower: the highest low
--   and the lowest high.
--
--   The unknown bounds are not really defined.  We concatenate them, for
--   what it's worth, which is at least sensible when none or only one of
--   them is defined, but not ideal when both are
narrow :: Bounds -> Bounds -> Bounds
narrow b1 b2 =
   b1 { bUnknown = bUnknown b1 ++ bUnknown b2
      , bLower   = mergeWith max (bLower b1) (bLower b2)
      , bUpper   = mergeWith min (bUpper b1) (bUpper b2)
      }
  where
    mergeWith _ Nothing    Nothing = Nothing
    mergeWith _ x@(Just _) Nothing = x
    mergeWith _ Nothing x@(Just _) = x
    mergeWith f (Just x)  (Just y) = Just (f x y)

-- ----------------------------------------------------------------------
-- * Number
-- ----------------------------------------------------------------------

-- | Fuzzy number is a variant on 'Number' that allows us the option
--   of overriding what would otherwise be singular agreement
--
--   If you don't need to, or have no idea why somebody would even want
--   to do such a thing, just 'defuzz' it
data FuzzyNumber = FN_Plural
                 | FN_MaybeSingular
                 | FN_Singular
  deriving (Eq, Show)

-- | @defuzz@ treats 'FN_MaybeSingular' as 'Singular'
defuzz :: FuzzyNumber -> Number
defuzz FN_Plural        = Plural
defuzz FN_MaybeSingular = Singular
defuzz FN_Singular      = Singular

-- | Somewhat abstract representation of subrx discriminators
--   (but in reality just based on English)
--
--   A discriminator is what we call the optional bit of text that helps
--   you distinguish one set instances of a class from another, eg,
--   “the same” or “another three”, or simply “the“.  This isn't a
--   technical term as far as I'm aware, just a made-up convenience word
data Discriminator = NilDiscriminator
                   | Bounded BoundsExpr
                   | TheSame
                   | TheOther
                   | TheOrdinal    Int
                   | NewOrdinal    Int
                   | Another       Int
                   | PlainCardinal Int
                   | CardinalOfThe Int
                   | The
  deriving (Eq, Show)

data BoundsExpr = SayAtLeast Int
                | SayAtMost  Int
                | SayBetween Int Int
                | SayExactly Int
                | SayArbitrary Text
  deriving (Eq, Show)
