module EvalModel where

import Control.Applicative
import Control.Lens
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Generics.Uniplate.Data
import Language.MiniZinc hiding (evalModel)
import Language.MiniZinc.Bindings
--import IPPrint
import Debug.Trace

--import Globals

evalModel :: Model -> Model
evalModel = evalModelWithBindings emptyBindings

evalModelWithBindings :: Bindings -> Model -> Model
evalModelWithBindings env = evalConstraints env . assigns

modelIsSatisfiable :: Bindings -> Model -> Bool
modelIsSatisfiable env = allConstraintsTrue . evalModelWithBindings env

modelIsSatisfiable' :: Bindings -> Model -> Bool
modelIsSatisfiable' env m =
    let m' = assigns m
    in allConstraintsTrue . evalModelWithBindings env $ m'

findUnsatisfiableConstraint :: Bindings -> Model -> (Maybe (Item))
findUnsatisfiableConstraint env m =
  let m' = assigns m
      env' = bindingsUnion (topLevelBindings m') env
      cons = [ ConstraintI e | ConstraintI e <- m' ^. modelItems ]
  in find ((/= (BoolLit True)) . view expRawExpression . unConstraintI . evalItem env') cons
  --     evaledCons = map (evalItem env') . map ConstraintI $ cons
  -- in find ((/= (BoolLit True)) . view expRawExpression . unConstraintI) evaledCons

unConstraintI (ConstraintI e) = e
unConstraintI _ = error "unConstraintI"

allConstraintsTrue :: Model -> Bool
allConstraintsTrue m =
  let constraints = [ e | ConstraintI e <- m ^. modelItems ]
  in case find ((/= (BoolLit True)) . view expRawExpression) constraints of
       Nothing -> True
       Just e -> False

-- evalItem :: Item -> Item
-- evalItem (VarDeclI

-- includes :: Model -> Model
-- includes m = m & modelItems %~ concatMap doInclude

-- doInclude (IncludeI path model) = undefined

assigns :: Model -> Model
assigns m = m & modelItems %~ mapMaybe (doAssign m)

doAssign m (VarDeclI vd) =
  let vd' = case vd ^. varDeclExpression of
              Just _ -> vd
              Nothing -> vd & varDeclExpression .~ lookupAssignI m (vd ^. varDeclIdent)
  in Just $ VarDeclI vd'
doAssign m (AssignI{}) = Nothing
doAssign m i = Just i

lookupAssignI m varid = listToMaybe (mapMaybe f (m ^. modelItems))
  where f (AssignI x rhs) | x == varid = Just rhs
        f _                            = Nothing

evalConstraints :: Bindings -> Model -> Model
evalConstraints env m =
  let env' = bindingsUnion (topLevelBindings m) env
  in m & modelItems %~ map (evalItem env')
--    let allBinds = allBindings m
--    m & modelItems %~ 

--allBindings m = foldl' (\b i -> addFunctionToBindings i b) (topLevelBindings m) (map generaliseDecorationItem globalFunctionItems)

evalItem :: Bindings -> Item -> Item
evalItem bs (ConstraintI e) = ConstraintI (makeExp $ expressionToValue bs (e ^. expRawExpression))
evalItem bs i = i

-- makeExp e' = Expression { _expRawExpression = e'
--                         , _expDecoration = mempty
--                         , _expAnnotations = mempty }


-- testModel2 = Model i
--     where i = [ IncludeI "stdlib.mzn"
-- --              , IncludeI "stdlib.
--               , ConstraintI (Call "alldifferent" [ ArrayLit [IntLit 1, IntLit 2] [(1,2)] ])
--               ]

-- testModel3 = Model i
--     where i = [ --IncludeI "stdlib.mzn"
-- --              , IncludeI "stdlib.
--                 ConstraintI (Call "alldifferent_except_0" [ ArrayLit [IntLit 0, IntLit 0, IntLit 2, IntLit 23] [(1,4)] ])
--               , AssignI "x" (IntLit 3)
--               , VarDeclI (VarDecl{_varDeclTypeInst = TypeInst (Type Par BTInt STPlain 0) Nothing [], _varDeclIdent = "x", _varDeclExpression = Nothing})
--               ]

-- main = pprint $ modelIsSatisfiable testModel3
