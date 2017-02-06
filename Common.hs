module Common where

import Control.Applicative
import Control.Lens
import Data.Data.Lens
import Data.List
import Language.MiniZinc

import Types

getErstwhileVariable :: Argument -> Maybe VarId
getErstwhileVariable (ErstwhileVariable vid) = Just vid
getErstwhileVariable _                       = Nothing

getIdentifier :: Argument -> Maybe VarId
getIdentifier (ErstwhileVariable vid) = Just vid
getIdentifier (OrdinaryParameter (Ident vid)) = Just vid
getIdentifier _                       = Nothing

argumentToExpression :: Argument -> Expression
argumentToExpression a = makeExp $ 
    case a of
      OrdinaryParameter e -> e
      ErstwhileVariable vid -> Ident $ vid
      ArgumentArrayAccess a idxs ->
          ArrayAccess (argumentToExpression a)
                      (map makeExp idxs)
      Blank                 -> error $ "argumentToExpression: Blank"

argumentToIdent (OrdinaryParameter (Ident i)) = Just i
argumentToIdent (ErstwhileVariable i) = Just i
argumentToIdent _ = Nothing


-- Strip a suffix from a list.
--
-- stripSuffix "def"    "abcdef" ==> Just "abc"
-- stripSuffix "abcdef" "abcdef" ==> Just ""
-- stripSuffix ""       "abcdef" ==> Just "abcdef"
-- stripSuffix "xyz"    "abcdef" ==> Nothing
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix xs = reverse <$> stripPrefix (reverse suffix) (reverse xs)


appearsInConstraint :: Model -> Argument -> Bool
appearsInConstraint m a =
    let i = case a of
              ErstwhileVariable v -> v
              OrdinaryParameter (Ident v) -> v
        cs = [ e | ConstraintI e <- m ^. modelItems ]
        e'sInCs = do c <- cs
                     e' <- universeOf template (c ^. expRawExpression)
                     return e'
    in Ident i `elem` e'sInCs

