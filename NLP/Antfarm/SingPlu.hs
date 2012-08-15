module NLP.Antfarm.SingPlu where

-- | Singular and Plural form of a noun
data SingPlu a = SP
    { sg :: a
    , pl :: a
    }
 deriving (Show, Eq)
