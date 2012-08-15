{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ( when )
import Control.Monad.State
import Data.List ( delete )
import Data.Text ( Text )
import Data.Tree
import System.Environment
import qualified Data.Set  as Set
import qualified Data.Text as T

import NLP.Minimorph.Util

import NLP.Antfarm.Demo
import NLP.Antfarm

main :: IO ()
main = do
  (verbose, args) <- do
      { as <- getArgs
      ; if "--verbose" `elem` as
           then return (True, delete "--verbose" as)
           else return (False, as)
      }
  case decode (unwords args) of
      Left err  -> fail $ "Did't understand the input: " ++ show err
      Right rxs ->
         let inps    = T.splitOn "," (T.pack (unwords args)) -- YUCK, duplication
             annoRxs = if length inps == length rxs
                          then zip inps rxs
                          else error "Argh, we wrote the antfarm parser wrong (length mismatch)"
         in putStr . T.unpack $ refexGen verbose annoRxs


refexGen :: Bool -> [(Text,[DiscourseUnit])] -> Text
refexGen verbose annoRxs =
    T.unlines $ output : details
  where
    rxs     = map snd annoRxs
    output  = T.intercalate ", " results
    results = evalState (mapM nextRx rxs) emptyHistory
    details = if verbose
                 then zipWith showDetails annoRxs results
                 else []

showDetails :: (Text, [DiscourseUnit]) -> Text -> Text
showDetails (x,dus) rx = T.intercalate "\t"
    [ T.strip x
    , T.intercalate "; " (map prettyTree dus)
    , rx
    ]
