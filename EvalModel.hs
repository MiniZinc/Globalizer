module EvalModel where

import Control.Lens
import Data.List
import Data.Maybe
import Language.MiniZinc hiding (evalModel)
import Language.MiniZinc.Bindings

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
      theCons = [ ConstraintI e | ConstraintI e <- m' ^. modelItems ]
  in find ((/= (BoolLit True)) . view expRawExpression . unConstraintI . evalItem env') theCons

unConstraintI :: Item -> Expression
unConstraintI (ConstraintI e) = e
unConstraintI _ = error "unConstraintI"

allConstraintsTrue :: Model -> Bool
allConstraintsTrue m =
  let constraints = [ e | ConstraintI e <- m ^. modelItems ]
  in case find ((/= (BoolLit True)) . view expRawExpression) constraints of
       Nothing -> True
       Just e -> False

assigns :: Model -> Model
assigns m = m & modelItems %~ mapMaybe (doAssign m)

doAssign :: Model -> Item -> Maybe Item
doAssign m (VarDeclI vd) =
  let vd' = case vd ^. varDeclExpression of
              Just _ -> vd
              Nothing -> vd & varDeclExpression .~ lookupAssignI m (vd ^. varDeclIdent)
  in Just $ VarDeclI vd'
doAssign m (AssignI{}) = Nothing
doAssign m i = Just i

lookupAssignI :: Model -> String -> Maybe Expression
lookupAssignI m varid = listToMaybe (mapMaybe f (m ^. modelItems))
  where f (AssignI x rhs) | x == varid = Just rhs
        f _                            = Nothing

evalConstraints :: Bindings -> Model -> Model
evalConstraints env m =
  let env' = bindingsUnion (topLevelBindings m) env
  in m & modelItems %~ map (evalItem env')

evalItem :: Bindings -> Item -> Item
evalItem bs (ConstraintI e) = ConstraintI (makeExp $ expressionToValue bs (e ^. expRawExpression))
evalItem bs i = i
