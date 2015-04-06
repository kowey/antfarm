{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns, TupleSections #-}
-- |
-- Copyright   : 2012 Eric Kow (Computational Linguistics Ltd.)
-- License     : BSD3
-- Maintainer  : eric.kow@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Discourse history tracking. The history module provides an
-- abstraction representation of what has already been said during refex
-- generation and a number of queries that would help the refex
-- generator to steer its decsions
module NLP.Antfarm.History where

import Control.Applicative ((<$>))
import Data.List ( foldl', elemIndex, delete )
import Data.Maybe ( isJust, mapMaybe )
import Data.Text ( Text )
import Data.Tree ( Tree(..), flatten )
import Prelude hiding ( lex )
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import NLP.Antfarm.Refex

-- | A 'RefGroup' is considered to refer exactly to its indices if it
--   has no bounds information or examples associated with it.
isExact :: RefGroup -> Bool
isExact rg = rgBounds rg == emptyBounds

-- | A discourse unit that would refer to just an element
mkSingletonDu :: RefKey -> DiscourseUnit
mkSingletonDu (c,i) = Node (RefGroup c (Set.singleton i) emptyBounds) []

{-
Note [discourse tree]
~~~~~~~~~~~~~~~~~~~~~

Our notion of a discourse unit is a tree of a RefGroup and its examples (and
their examples, etc).  When we enter such a unit into the history, we also
enter all of the examples as separate entries at the same time. This
facilitates queries on the units, and also affects the counts.  For example, if
we say “two atoms (a carbon and an oxygen)” we can consider the oxygen to have
already beeen referenced once so when we bring up another oxygen, we say
“another oxygen” accounting for the fact that oxygen had already been mentioned
once
-}
instance Ord (Tree RefGroup) where
     compare (Node r1 ks1) (Node r2 ks2) =
         case compare r1 r2 of
             EQ -> compare ks1 ks2
             x  -> x

-- | The discoure history keeps track of decisions we have already made
--   about how to realise previous parts of the referring expression.
--   This sort of information can help us to make more fluent decisions
--   on how to realise the current fragment
data RefHistory  = RefHistory
    { -- | How many times a 'DiscourseUnit' has been mentioned
      rhCount :: Map.Map DiscourseUnit RefCount
      -- | For each class: an ordering of indices that reflects what
      --   ordinal expression should be used for them (if at all)
      --
      --   So @[c8,c3,c4]@ means
      --
      --   c8: the first
      --   c3: the second
      --   c4: the third
    , rhOrder :: Map.Map Text [Text]
    }
type RefCount    = Int

-- | Discourse history without any objects
emptyHistory :: RefHistory
emptyHistory = RefHistory Map.empty Map.empty

-- ----------------------------------------------------------------------
-- * building the discourse history
-- ----------------------------------------------------------------------

-- See note `discourse tree'
--
-- | Take note of the fact that these discourse units have been mentioned
--   (again) in the history.
--
--   You probably want to realise the units first, then add them to the
--   history.
addToHistory :: [DiscourseUnit] -> RefHistory -> RefHistory
addToHistory ks st = st
    { -- incr number of mentions for each mention of an
      -- entity in the expression
      rhCount = foldl' plusOne    (rhCount st) (concatMap subtrees ks)
      -- note ordinal position (eg. 1st, 2nd) of any new singleton
      -- instances of each entity type we see
    , rhOrder = foldl' addOrdinal (rhOrder st) (concatMap duSingletons ks)
    }
  where
    -- | incr ref counts of 'k' in 'm'
    plusOne    m k = Map.insertWith' (+) k 1 m
    -- | note ordinal position of instances 'i' (relative to other members
    --   of class 'c'), but only if this is the first mention of it
    addOrdinal m (c,i) =
        Map.insertWith' append c [i] m
      where
        append new old = if all (`elem` old) new then old else old ++ new

-- | Individuals mentioned in a discourse unit
--   (see 'refSingleton')
duSingletons :: DiscourseUnit -> [RefKey]
duSingletons =
    mapMaybe refSingleton . flatten

-- | A 'refSingleton' is an instance that appears by itself in a 'RefGroup'
--   without other items or constraints that imply that there could be other
--   items
refSingleton :: RefGroup -> Maybe RefKey
refSingleton (RefGroup c is bnds) =
    case Set.toList is of
        [i] -> if kosher bnds then Just (c,i) else Nothing
        _   -> Nothing
  where
    -- either no bounds information, or “exactly 1" are acceptable
    kosher (Bounds [] Nothing Nothing)   = True
    kosher (Bounds [] (Just 1) (Just 1)) = True
    kosher _ = False

-- | If a RefGroup has explicit constraints, augment them with the
--   implicit constraints that arise from treating each item
--   as evidence of an at least constraint
--
--   It's a good idea to run this once when building 'RefGroup's,
--   but you may also decide that this sort of behaviour is not
--   desirable for your application, so it's off by default
noteImplicitBounds :: RefGroup -> RefGroup
noteImplicitBounds rg =
     if implicits > 0
        then rg { rgBounds = bounds2 }
        else rg
   where
     bounds1 = rgBounds rg
     bounds2 =
         -- We only ever modify the lower bound constraints:
         --
         -- * if there's a lower bound constraint, raise it to
         --   the number of implicits (as needed)
         -- * otherwise, set the lower bound (but only if we
         --   already have an explicit constraint, in the form
         --   of an upper bound)
         --
         -- Note also we don't bother to verify that upper bounds
         -- make sense wrt their lower bounds
         case bLower bounds1 of
             Just l  -> bounds1 { bLower = Just (max l implicits) }
             Nothing -> if isJust (bUpper bounds1)
                           then bounds1 { bLower = Just implicits }
                           else bounds1
     implicits = Set.size (rgIdxes rg)

-- ----------------------------------------------------------------------
-- * query
-- ----------------------------------------------------------------------

-- | @hasDistractorGroup st k@ returns whether or not the discourse history
--   @st@ contains a group with distractors to @k@.
--
--   See 'distractorGroups' for more details
hasDistractorGroup :: RefHistory -> RefKey -> Bool
hasDistractorGroup st k =
    not . null $ distractorGroups st k

-- | @distractorGroups st k@ returns all the distractor groups for @k@
--   in the discourse history.
--
--   A distractor is defined (here) as something that has the the same class
--   as @k@ but a different index, basically anything that might be confused
--   for the object in question
distractorGroups :: RefHistory -> RefKey -> [DiscourseUnit]
distractorGroups st (c, i) =
    filter distracting (Map.keys (rhCount st))
  where
    distracting = not . safe
    safe (Node rg2 _) = c /= rgClass rg2 -- different classes (OK)
                     || i `Set.member` rgIdxes rg2 -- includes me (OK)
                     || isClasswide rg2 -- classes aren't instances (OK)

-- | @hasSupersetMention st g@ returns whether or not the discourse history
--   contains a group that includes all members of @g@
--
--   Note that if a group has already occured in the discourse history, this
--   returns a True (ie. not a strict superset)
hasSupersetMention :: RefHistory -> DiscourseUnit -> Bool
hasSupersetMention st k = not . Map.null . rhCount $ supersetMentions k st

-- | @supersetMentions g st@ returns the portion of discourse history @st@
--   in which all groups are supersets of @g@ (inclusive, not strict super)
supersetMentions :: DiscourseUnit -> RefHistory -> RefHistory
supersetMentions (Node g _) h =
    h { rhCount = Map.filterWithKey hasK (rhCount h) }
  where
    hasK (Node g2 _) _ =
        rgClass g == rgClass g2 &&
        rgIdxes g `Set.isSubsetOf` rgIdxes g2

-- | @lastMention st k@ returns the number of times @k@ has been mentioned
lastMention :: RefHistory -> RefKey -> Int
lastMention st (c,i) =
    sum . Map.elems . rhCount $ supersetMentions (mkSingletonDu (c,i)) st

-- | @lastMention st g@ returns the number of times the group @g@ has been
--   mentioned
lastMentions :: RefHistory -> DiscourseUnit -> Int
lastMentions st k = Map.findWithDefault 0 k (rhCount st)

-- | True if the given instance @k@ has never been mentioned before
isFirstMention :: RefHistory -> RefKey -> Bool
isFirstMention st k = lastMention st k == 0

-- ----------------------------------------------------------------------
-- * subtle queries
-- ----------------------------------------------------------------------

-- | If it makes sense to refer to a key using an ordinal expression,
--   the order we should assign it (Nothing if we either can't sensibly
--   assign one, or the history does not give us enough information to
--   do so)
mentionOrder :: RefHistory -> RefKey -> Maybe Int
mentionOrder rh (c,i) =
   if isOnlySingletons rh
      then case Map.lookup c (rhOrder rh) of
               -- +1 is for natural language (we don't say "zeroth")
               Just is | length is > 1 -> (+ 1) <$> elemIndex i is
               _                       -> Nothing
      else Nothing
  where
     -- the c's never appear with others of their own kind in the same
     -- 'RefGroup' [we want to be able to say things like "the 5th ant"
     -- but we haven't yet worked out how this would work if 'ant' has
     -- already appeared in expressions like "the 3 ants"... in that
     -- case, what does 5th mean?]
     isOnlySingletons = not . any isMultiMatch
                      . Map.keys
                      . rhCount
     -- if a refgroup is of the given class (and has more than one elm)
     isMultiMatch (Node g _) =
         c == rgClass g && Set.size (rgIdxes g) > 1

-- | Is a subset of a previously mentioned group @g@ where there are no
-- distractors to @g@ in the discourse history
hasTidyBackpointer :: RefHistory -> DiscourseUnit -> Bool
hasTidyBackpointer st du@(Node rg _) =
    not (any (hasDistractorGroup st) keys) -- (tidy) nothing to confuse w du
    && lastMentions st du == 0  -- (tidy) first time we mentioned du
    && hasSupersetMention st du -- (backpointer) but we already mentioned
                                -- a group that includes all members of du
  where
    -- keys are just class/idx tuples ('a', '3') for example
    keys = Set.toList (refKeys rg)

-- | @isTheOther st k@ returns whether or not there is a two-member group in
--   the discourse history which @k@ is a member of such that the other
--   member has already been mentioned as a part of a singleton group.
--
--   The idea is that if you have said "one of the X", you will want to say
--   "the other X" for the other member of that group
isTheOther :: RefHistory -> RefKey -> Bool
isTheOther st (c,i) =
    any isBuddy $ Map.keys (rhCount st)
  where
    isBuddy (Node g2 []) | isExact g2 && c == rgClass g2 =
        -- the other instance was mentioned at least once
        case getBuddy (rgIdxes g2) of
          Just b  -> lastMentions st (mkSingletonDu (c,b)) >= 1
          Nothing -> False
    isBuddy _ = False
    -- only trigger this for classes where they've been exactly
    -- two instances ["the ant, the other ant" is fine, but
    -- "the ant, a second ant, the other ant" is confusing...
    -- which other ant?]
    getBuddy (Set.toList -> idx) | length idx == 2 =
         case delete i idx of
             [b] -> Just b -- must be *exactly* one other item
             _   -> Nothing
    getBuddy _ = Nothing

-- | Is the class itself, not any individual entity within that class
--   ie. “ants” instead of “an ant” or “some ants”
--
--   By convention, any group which containts no indices or constraints
--   is considered to be classwide.
isClasswide :: RefGroup -> Bool
isClasswide rg = Set.null (rgIdxes rg) && isExact rg

-- ----------------------------------------------------------------------
-- * odds and ends
-- ----------------------------------------------------------------------

-- | Like 'flatten', but returns whole subtrees instead of
--   just nodes:
--
--   > a(b c(d e(f g)) h)
--   > b
--   > c(d e(f g))
--   > d
--   > e(f g)
--   > f
--   > g
--   > h
--
--  Invariant: @map rootLabel (subtrees x) == flatten x@
subtrees :: Tree a -> [Tree a]
subtrees t =
    grab t []
  where
    grab st@(Node _ ts) xs = st : foldr grab xs ts

mkLeaf :: a -> Tree a
mkLeaf x = Node x []

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z
