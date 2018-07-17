{-# LANGUAGE ViewPatterns #-}

module Bindings where

import Control.Lens
import Data.Maybe
import Language.MiniZinc
import Common
import Types

isArray :: Bindings -> Argument -> Bool
isArray _env (OrdinaryParameter (ArrayLit _ _)) = True
isArray env x =
  maybe False typeInstIsArray
        (lookupTypeInst (fromMaybe (error "1") (getIdentifier x)) env)
is1DArray :: Bindings -> Argument -> Bool
is1DArray _env (OrdinaryParameter (ArrayLit _ [(_,_)])) = True
is1DArray env (ArgumentArrayAccess a args) =
    is2DArray env a && (length (filter (== (Ident "_")) args)) == 1
is1DArray env x = 
  maybe False typeInstIs1DArray
          (getIdentifier x >>= \i -> lookupTypeInst i env)

is2DArray :: Bindings -> Argument -> Bool
is2DArray _ (ArgumentArrayAccess _ _) = False
is2DArray env x =
  maybe False typeInstIs2DArray
        (lookupTypeInst (fromMaybe (error ("is2DArray: " ++ show x)) (getIdentifier x)) env)

is3DArray :: Bindings -> Argument -> Bool
is3DArray _ (ArgumentArrayAccess _ _) = False
is3DArray env x =
  maybe False typeInstIs3DArray
        (lookupTypeInst (fromMaybe (error ("is3DArray: " ++ show x)) (getIdentifier x)) env)

isInt :: Bindings -> Argument -> Bool
isInt _   (OrdinaryParameter (IntLit _)) = True
isInt env (OrdinaryParameter (ArrayAccess (expRaw -> Ident a) [idx])) =
    is1DArray env (OrdinaryParameter (Ident a)) && isInt env (OrdinaryParameter (idx ^. expRawExpression))
isInt env (ArgumentArrayAccess a args) = isArray env a && length (filter (== (Ident "_")) args) == 0
isInt env x = fromMaybe False $ do
                vid <- getIdentifier x
                ti <- lookupTypeInst vid env
                return $ typeInstIsInt ti

typeInstIsArray,typeInstIs1DArray,typeInstIs2DArray,typeInstIs3DArray,typeInstIsVar
  :: TypeInst -> Bool
typeInstIsArray (TypeInst { tiRanges = OrdinaryRanges rs }) = length rs > 0
typeInstIsArray _ = error "typeInstIsArray"
typeInstIs1DArray (TypeInst { tiRanges = OrdinaryRanges rs }) = length rs == 1
typeInstIs1DArray _ = error "typeInstIs1DArray"
typeInstIs2DArray (TypeInst { tiRanges = OrdinaryRanges rs }) = length rs == 2
typeInstIs2DArray _ = error "typeInstIs2DArray"
typeInstIs3DArray (TypeInst { tiRanges = OrdinaryRanges rs }) = length rs == 3
typeInstIs3DArray _ = error "typeInstIs3DArray"
typeInstIsVar ti | tiInst ti == Var = True
typeInstIsVar _                               = False

typeInstIsInt :: TypeInst -> Bool
typeInstIsInt ti | tiBase ti == BTInt && tiSet ti == Plain && tiRanges ti == OrdinaryRanges [] = True
typeInstIsInt _                                       = False

isPar :: VarDecl -> Bool
isPar = not . typeInstIsVar . view varDeclTypeInst
