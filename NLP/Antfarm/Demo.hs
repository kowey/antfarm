{-# LANGUAGE OverloadedStrings, TupleSections #-}
-- |
-- Copyright   : 2012 Eric Kow (Computational Linguistics Ltd.)
-- License     : BSD3
-- Maintainer  : eric.kow@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Helper functions for the antfarm demonstrator.  You probably don't want to
-- import this module unless you're doing something amusing like making a web
-- app out of the antfarm demonstrator.  But it could be useful to look at the
-- source if you're making something using antfarm
module NLP.Antfarm.Demo where

import Control.Applicative
import Control.Arrow hiding ( (<+>) )
import Control.Monad.Trans.State
import Control.Monad.Identity
import Data.Char ( isAlpha, isSpace, isDigit )
import Data.Function
import Data.List
import Data.List ( find, nub )
import Data.Maybe
import Data.Text ( Text )
import Data.Tree
import qualified Data.Set as Set
import qualified Data.Text as T

import NLP.Minimorph.English
import NLP.Minimorph.Number
import NLP.Minimorph.Util
import Text.Parsec hiding ( State )
import Text.Parsec.String
import qualified Text.Parsec as P

import NLP.Antfarm
import NLP.Antfarm.English
import NLP.Antfarm.History
import NLP.Antfarm.Refex
import NLP.Antfarm.Cardinality

decode :: String -> Either ParseError [[DiscourseUnit]]
decode t =
    map fromDemoForest <$> parse (pFilled pSentence) "" t

decodeRx :: String -> Either ParseError [DiscourseUnit]
decodeRx t =
    fromDemoForest <$> parse (pFilled pDemoElemForest) "" t

type RefStateT m a = StateT RefHistory m a
type RefState a    = RefStateT Identity a

nextRx :: Monad m => [DiscourseUnit] -> RefStateT m Text
nextRx dus = do
   oldst <- get
   modify (addToHistory dus)
   return $ englishRx $ rx oldst (map toSubRxInput dus)

-- ----------------------------------------------------------------------
--
-- ----------------------------------------------------------------------

itemToClass :: Text -> Text
itemToClass i_ =
    fromMaybe i (lookup i lexMap)
  where
    i = stripNonClassStuff i_

isClassWide :: Text -> Bool
isClassWide i = i == stripNonClassStuff i

stripNonClassStuff :: Text -> Text
stripNonClassStuff = T.takeWhile isAlpha

lexMap :: [ (Text,Text) ]
lexMap = [ ("a", "ant")
         , ("b", "box")
         , ("c", "cat")
         , ("d", "dog")
         , ("e", "egg")
         , ("f", "fox")
         , ("g", "gun")
         , ("h", "hen")
         , ("i", "imp")
         , ("j", "jug")
         , ("k", "key")
         , ("l", "leg")
         , ("m", "map")
         , ("n", "nun")
         , ("o", "owl")
         , ("p", "pig")
         , ("q", "car")
         , ("r", "rig")
         , ("s", "saw")
         , ("t", "tin")
         , ("u", "ubi")
         , ("v", "vat")
         , ("w", "wig")
         , ("x", "axe")
         , ("y", "yew")
         , ("z", "zoo")
         , ("A", "animal")
         , ("B", "bird")
         , ("M", "mammal")
         ]

onWords :: (Text -> Maybe Text) -> Text -> Text
onWords f = T.unwords . mapMaybe f . T.words

intercalateRx :: [Text] -> Text
intercalateRx = T.intercalate ", "


-- ----------------------------------------------------------------------
-- aura-rx-test language examples
--
-- a1
-- a1 a2
-- a1 a2
-- a >= 5 a3
-- a >= 5 ( b1 b3 )
-- a1 a2  ( b1 b3 ) c < 3 ( d3 )
-- ----------------------------------------------------------------------

data DemoElem = DemoElem
    { dClass  :: Text
    , dConstr :: Constr
    }
  deriving Show

data Constr = Constr Constraint
            | Inst   Text
            | ClassWide
  deriving Show

fromDemoElem :: DemoElem -> RefGroup
fromDemoElem e = RefGroup
    { rgClass  = dClass e
    , rgIdxes  = Set.fromList   [ x | Inst x   <- [dConstr e] ]
    , rgBounds = explicitBounds [ x | Constr x <- [dConstr e] ]
    }

mergeGroups :: [RefGroup] -> RefGroup
mergeGroups [] =
    error $ "mergeGroups: can't merge empty list"
mergeGroups rs@(r0:_) =
    if any (\r -> rgClass r /= rgClass r0) rs
       then error . T.unpack $
                "mergeGroups: not all constraint classes match" <+> rgClass r0
       else noteImplicitBounds $
            r0 { rgIdxes  = Set.unions    (map rgIdxes  rs)
               , rgBounds = foldr1 narrow (map rgBounds rs)
               }

-- | Regroup constraints and examples so that like are with like
--
-- > a1 b3 a4
-- >   ==> [a1 a4] [b3]
-- > a1 b3 a4 (x <= 3) b6
-- >   ==> [a1 a4]([x <= 3]) [b3 b6]
-- > a1 b3 a4 (x <= 3) b6 (y >= 8) a <= 1 (x >= 8)
-- >   ==> [a1 a4]([x <= 3 >= 8]) [b3 b6]([y>=8])
--
fromDemoForest :: [Tree DemoElem] -> [DiscourseUnit]
fromDemoForest ts =
    map mergeChunk chunks
  where
    key     = dClass . rootLabel
    -- tries to preserve order
    chunks  = map findChunk $ nub $ map key ts
    chunks_ = buckets key ts
    findChunk t = maybe (error "fromDemoForest: resort oops") (t,)
                $ lookup t chunks_
    --
    mergeChunk (_, xs) =
         Node (mergeGroups (map convert xs))
              (fromDemoForest $ concatMap subForest xs)
    convert = fromDemoElem . rootLabel

toSubRxInput :: DiscourseUnit -> Tree SubRxInput
toSubRxInput =
    onSubTrees helper
  where
    helper du@(Node rg _) = SubRxInput
        { srxInpDet    = SP [indefiniteDet word] ["the"]
        , srxInpWord   = SP word (defaultNounPlural word)
        , srxInpEntity = du
        }
      where
        word = rgClass rg

-- ----------------------------------------------------------------------
-- Parser for rx language
-- ----------------------------------------------------------------------

pFilled :: Parser a -> Parser a
pFilled p = spaces *> p <* eof

pSentence :: Parser [[Tree DemoElem]]
pSentence = (spaces *> pDemoElemForest) `sepBy` char ','

pDemoElemForest :: Parser [Tree DemoElem]
pDemoElemForest = pDemoElemTree `sepEndBy` spaces

pDemoElemTree :: Parser (Tree DemoElem)
pDemoElemTree = do
    n    <- pDemoElem ; spaces
    kids <- option [] $ P.between (char '(') (char ')') $
                pDemoElemForest
    return (Node n kids)

pDemoElem :: Parser DemoElem
pDemoElem = do
   lx      <- pLexeme <* spaces
   mconstr <- option Nothing (Just <$> pConstr)
   let inst = case mconstr of
           Nothing | isClassWide lx -> ClassWide
                   | otherwise      -> Inst lx
           Just c                   -> Constr c
   return $ DemoElem (itemToClass lx) inst

pConstr :: Parser Constraint
pConstr = try $ do
   op <- pOp
   case getOp op of
       Nothing -> fail . T.unpack $ "not a known op:" <+> op
       Just fn -> spaces *> (fn <$> pNatural)
 where
   getOp o = fst <$> find ((o `elem`) . snd) opTable


opTable :: [(Int -> Constraint, [Text])]
opTable =
   [ (AtLeast,         [">=", "ge", "at-least"])
   , (AtLeast . (1+),  [">" , "gt"])
   , (AtMost,          ["<=", "le", "at-most"])
   , (AtMost . minus1, ["<" , "lt"])
   , (Exactly,         ["==", "=", "eq", "exactly"])
   ]
  where
    minus1 x = x - 1

pLexeme :: Parser Text
pLexeme =
    T.pack <$> many1 (satisfy isLexChar)
  where
    isLexChar c = c `notElem` "()<=>," && not (isSpace c)

pOp :: Parser Text
pOp =
    T.pack <$> many1 (satisfy isLexChar)
  where
    isLexChar c = c `notElem` "()," && not (isSpace c || isDigit c)

pNatural :: Parser Int
pNatural = read <$> many1 digit

-- ----------------------------------------------------------------------
--
-- ----------------------------------------------------------------------

class Pretty a where
    pretty :: a -> Text

instance Pretty RefGroup where
    pretty (RefGroup cl idxs bs) =
        cl <+> (parens . T.unwords $ Set.toAscList idxs)
           <+> pretty bs

instance Pretty Bounds where
    pretty (Bounds bs ml mu) =
        maybe "" ge ml <+> maybe "" le mu <+>
        (if null bs then "" else squares (T.unwords bs))
      where
        le i = "≤" <> pretty i
        ge i = "≥" <> pretty i

instance Pretty Text where
    pretty = id

instance Pretty Int where
    pretty = T.pack . show

parens :: Text -> Text
parens t = "(" <> t <> ")"

squares :: Text -> Text
squares t = "(" <> t <> ")"

prettyForest :: Pretty a => [Tree a] -> Text
prettyForest = T.unwords . map prettyTree

prettyTree :: Pretty a => Tree a -> Text
prettyTree (Node x []) = pretty x
prettyTree (Node x ns) = pretty x <+> "(" <> prettyForest ns <> ")"

-- ----------------------------------------------------------------------
-- odds and ends
-- ----------------------------------------------------------------------

buckets :: Ord b => (a -> b) -> [a] -> [ (b,[a]) ]
buckets f = map (first head . unzip)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)
          . map (\x -> (f x, x))

onSubTrees :: (Tree a -> b) -> Tree a -> Tree b
onSubTrees f n@(Node _ ks) = Node (f n) (map (onSubTrees f) ks)
