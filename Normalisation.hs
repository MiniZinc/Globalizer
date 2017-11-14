{-# LANGUAGE ScopedTypeVariables #-}

module Normalisation where

import Control.Lens
import Control.Monad
import Data.Data.Lens
import Data.List
import Data.Maybe
import Language.MiniZinc
import Language.MiniZinc.Bindings

import Loc
import Transform


afterDataNormalisation ::[ Model -> Maybe (Model) ]
afterDataNormalisation =
             [ transformConstraintExpressions collectForalls
             , monadToMaybe.transformMOnOf (template :: Traversal' (Model) [Generator]) template (monadify separateGenerators)
             , splitConjunctions2
             , removeTrivialConstraints
             , transformConstraintExpressions splitConjunctionInForall
             , assignItemsToVarDecls
             , removeDuplicateVarDecls
             ]

initialNormalisation ::[ Model -> Maybe (Model) ]
initialNormalisation =
    [ transformConstraintExpressions collectForalls
    , monadToMaybe.transformMOnOf (template :: Traversal' (Model) [Generator]) template (monadify separateGenerators)
    , splitConjunctions2
    , removeTrivialConstraints
    , transformConstraintExpressions splitConjunctionInForall
    , inlineCalls
    ]

rewriteModel :: [ Model -> Maybe (Model) ] -> Model -> Model
rewriteModel rewritings = rewriteOf ignored (\x -> msum [ r x | r <- rewritings ])

-- Separate generators like so:
--
-- forall (i,j in S) (body)
--   =>
-- forall (i in S, j in S) (body)
--
-- Technically this may be dangerous due to identifier collisions.
separateGenerators :: [Generator] -> Maybe [Generator]
separateGenerators gens =
    let gens' = concatMap f gens
    in if length gens' == length gens
       then Nothing
       else Just gens'
  -- We throw away the location here ("Nothing") because it's no longer correct.
  where f (Generator vds is _) = [Generator [vd] is Nothing | vd <- vds]


-- Inline predicate calls.
inlineCalls :: Model -> Maybe (Model)
inlineCalls m =
  monadToMaybe . transformMOnOf template template (monadify (inlineCallsExp env)) $ m
  where
    env = topLevelBindings m


inlineCallsExp :: Bindings -> Expression -> Maybe (Expression)
inlineCallsExp env e = do
  Call funcIdent args <- return $ e ^. expRawExpression
  funcItems <- lookupFunction funcIdent env
  FunctionI _ _ vds _ body <- find isFunctionIWithBody funcItems
  let vds' = zipWith (\vd a -> vd & varDeclExpression .~ Just a) vds args
      body' = fromMaybe (error $ "inlineCallsExp: function " ++ show funcIdent ++ " has no body") body
  return $ makeExp $ Let (map VarDeclI vds') body'

-- If a constraint has a conjunction at the top level, split it into
-- two constraints.
--
-- constraint A /\ B;
--   =>
-- constraint A;
-- constraint B;
splitSingleConstraint2 :: Item -> Maybe [Item]
splitSingleConstraint2 (ConstraintI e) =
    case e ^. expRawExpression of
      BinOp e1 BinOpAnd e2 -> Just [ConstraintI e1, ConstraintI e2]
      _ -> Nothing
splitSingleConstraint2 _ = Nothing

-- View each item as a singleton list.
singletonItems :: Iso' [Item] [[Item]]
singletonItems = iso (map (:[])) concat

-- Given a transformation which operates on a single item but can
-- return zero-or-more items, apply this transformation to every item
-- in the model.  If the transformation returns zero items, the item
-- disappears; if it returns many items, they are inserted into the
-- model.  In every pass the transformation will be applied to every
-- item.
transformSingletonItems :: ([Item] -> Maybe [Item]) -> Model -> Maybe (Model)
transformSingletonItems f = 
    monadToMaybe
    . transformMOnOf
          (modelItems.singletonItems.traverse)
          ignored
          (monadify f)

-- If we get a singleton list, pass it to "f"; otherwise, return
-- Nothing.
acceptSingleton :: (a -> Maybe [b]) -> [a] -> Maybe [b]
acceptSingleton  f [x] = f x
acceptSingleton _f _   = Nothing

splitConjunctions2 :: Model -> Maybe (Model)
splitConjunctions2 = transformSingletonItems (acceptSingleton splitSingleConstraint2)

removeTrivialConstraints :: Model -> Maybe (Model)
removeTrivialConstraints = monadToMaybe
                           . transformMOnOf
                                 (modelItems.singletonItems.traverse)
                                 ignored
                                 (monadify removeTrivialConstraint)
removeTrivialConstraint :: [Item] -> Maybe [Item]
removeTrivialConstraint [ConstraintI e] =
    case e ^. expRawExpression of
      BoolLit True -> Just []
      _ -> Nothing
removeTrivialConstraint _ = Nothing

removeDuplicateVarDecls :: Model -> Maybe (Model)
removeDuplicateVarDecls m =
    let previousItems = m ^. modelItems
        newItems = nub previousItems
    in if length newItems == length previousItems
       then Nothing
       else Just $ m & modelItems .~ newItems

constraintExpression :: Prism' (Item) (Expression)
constraintExpression = prism ConstraintI f
  where f (ConstraintI e) = Right e
        f i = Left i

-- Groups together foralls like so:
--
-- forall (i in S where X)
--   (forall (j in T where Y)
--     (body))
--
--  =>
--
-- forall (i in S, j in T where X /\ Y)
--   (body)
--
-- This does NOT take clashing identifiers into account!
collectForalls :: Expression -> Maybe (Expression)
collectForalls e = do
  GenCall "forall" c1 <- return $ e ^. expRawExpression
  GenCall "forall" c2 <- return $ c1 ^. compBody ^. expRawExpression
  guard (c1 ^. compType == ArrayComprehension)
  guard (c2 ^. compType == ArrayComprehension)
  let w1 = fromMaybe (locExp $ BoolLit True) $ c1 ^. compWhere
      w2 = fromMaybe (locExp $ BoolLit True) $ c2 ^. compWhere
  let newComp = Comprehension {
                     _compBody = c2 ^. compBody
                   , _compGens = c1 ^. compGens ++ c2 ^. compGens
                   , _compWhere = Just (locExp $ BinOp w1 BinOpAnd w2)
                   , _compType = ArrayComprehension
                   }
  return $ locExp $ GenCall "forall" newComp

transformConstraintExpressions :: (Expression -> Maybe (Expression))
                               -> (Model -> Maybe (Model))
transformConstraintExpressions f =
    monadToMaybe
    . transformMOnOf
        (modelItems.traverse.constraintExpression)
        ignored
        (monadify f)

-- Split a conjunction inside a forall like so:
--
-- forall (i in S where X) (A /\ B)
--   =>
--    forall (i in S where X) (A)
-- /\ forall (i in S where X) (B)
splitConjunctionInForall :: Expression -> Maybe (Expression)
splitConjunctionInForall e = do
  GenCall "forall" c1 <- return $ e ^. expRawExpression
  guard (c1 ^. compType == ArrayComprehension)
  BinOp b1 BinOpAnd b2 <- return $ c1 ^. compBody ^. expRawExpression
  return $ locExp $ BinOp (locExp $ GenCall "forall" (c1 & compBody .~ b1))
                           BinOpAnd 
                           (locExp $ GenCall "forall" (c1 & compBody .~ b2))


allEq :: Eq a => [a] -> Bool
allEq (x:xs) = all (==x) xs
allEq _ = True


assignItemsToVarDecls :: Model -> Maybe (Model)
assignItemsToVarDecls m = do
  AssignI vid e <- find isAssignI (m ^. modelItems)
  let matches (VarDeclI vd) = vd ^. varDeclIdent == vid
      matches _ = False
      attach (VarDeclI vd) = VarDeclI $ vd & varDeclExpression .~ Just e
      attach _i = error "assignItemsToVarDecls / attach"
      newItems = delete (AssignI vid e) $
            m ^. modelItems & traverse . filtered matches %~ attach
  return $ m & modelItems .~ newItems


