module Common where

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
      ArgumentArrayAccess aa idxs ->
          ArrayAccess (argumentToExpression aa)
                      (map makeExp idxs)
      Blank                 -> error $ "argumentToExpression: Blank"

argumentToIdent :: Argument -> Maybe String
argumentToIdent (OrdinaryParameter (Ident i)) = Just i
argumentToIdent (ErstwhileVariable i) = Just i
argumentToIdent _ = Nothing

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

