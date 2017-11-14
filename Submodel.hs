{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Submodel where

import Control.Applicative
import Control.Lens hiding (Context)
import Control.Monad
import Data.Either
import Data.List
import qualified Data.Text as T
import Language.MiniZinc
import Language.MiniZinc.Resolve
import System.Environment

import Loc
import Normalisation
import Statistics

type Context = Maybe (Expression)
type Submodel = (Model, Context)

-- The submodels of a model, subject to a maximum number of constraint
-- items.  The context does not count towards this maximum.
submodels :: Int -> Model -> [ Submodel ]
submodels maxConstraints m = do
  -- Split the constraint items from the rest.
  let (constraintItems, otherItems) = partition isConstraintI (m ^. modelItems)
  -- Choose a subset of the constraint items.
  (selected, discarded) <- chooseSubset maxConstraints constraintItems
  guard (not (null selected))
  -- Choose a (possibly null) context from the unused constraints.
  context <- Nothing : map (\ (ConstraintI e) -> Just e) discarded

  let submodel = Model $ otherItems ++ selected
  return (submodel, context)

-- Choose a subset from xs, where the chosen subset's size is at most
-- n.  Returns both the chosen subset and the remainder.
chooseSubset _ [] = return ([], [])
chooseSubset 0 xs = return ([], xs)
chooseSubset n (x:xs) =
  -- Exclude x.
  (do (s,d) <- chooseSubset n xs
      return (s,x:d))
  <|>
  -- Include x.
  (do (s,d) <- chooseSubset (n-1) xs
      return (x:s,d))


-- Given a constraint expression, return all its possible variants.
-- The variants include the constraint unchanged, and any unrollings
-- of foralls.  Each result is accompanied by the set of variables
-- that were unrolled, and the set from which their values should be
-- drawn.
--
-- E.g.
-- newvariants "forall (i in S, j in T) (b)"
--   =>
-- ("forall(i in S,j in T) (b)", [])
-- ("b",                         [("int: i","S"),("int: j","T")])
-- ("forall(j in T) (b)",        [("int: i","S")])
-- ("forall(i in S) (b)",        [("int: j","T")])

newvariants :: Expression -> [ (Expression, [(VarDecl, Expression)]) ]
newvariants e = basic ++ unrolled
  where
    basic = [ (e, []) ]
    unrolled = do GenCall "forall" c <- return $ e ^. expRawExpression
                  (gensToUnroll, otherGens) <- partitions (c ^. compGens)
                  -- At least one thing must be unrolled.
                  guard (not (null gensToUnroll))
                  -- The domain of an unrolled gen must not refer to
                  -- the identifier of a not-unrolled gen.
                  -- XXX: a bit crude if there are reused identifiers
                  let unrolledDomains = map (view genExp) gensToUnroll
                      usedIdentifiers = concatMap unboundIdentifiersAfterResolution unrolledDomains
                      notunrolledIdentifiers = [ i | g <- otherGens, vd <- g ^. genVarDecls, let i = vd ^. varDeclIdent ]
                  let badIdent = find (`elem` notunrolledIdentifiers) usedIdentifiers
                  guard $ case badIdent of
                            Nothing -> True
                            -- Just i -> unsafePerformIO $ do
                            --               hPutStrLn stderr $ "BAD IDENTIFIER: " ++ show i
                            --               hPutStrLn stderr $ showExp e
                            --               hPutStrLn stderr $ "unrolled domains: " ++ intercalate ", " (map showExp unrolledDomains)
                            --               hPutStrLn stderr $ "not unrolled: " ++ show notunrolledIdentifiers
                            --               return False
                            Just _i -> False
                  let e' =
                         -- There are two cases to consider: we have
                         -- unrolled the forall entirely or not.  In
                         -- the first case, the forall disappears and
                         -- its where clause becomes the condition of
                         -- an if-then-else expression.  In the second
                         -- case we preserve the forall with the
                         -- not-unrolled variables.
                         --
                         -- E.g.
                         --   forall (i in S, j in T where X) (body)
                         -- If we unroll both i and j we get:
                         --   if X then body else true
                         -- If we unroll only i we get:
                         --   forall (j in T where X) (body)
                         if null otherGens
                         then maybe id (\wc ee -> locExp $ ITE [(wc, ee)] (locExp $ BoolLit True)) (c ^. compWhere) (c ^. compBody)
                         else locExp $ GenCall "forall" (c & compGens .~ otherGens)
                  return (e', map f gensToUnroll)
    f (Generator vds is _loc) =
        case vds of
          [] -> error "newvariants/f: generator with no vardecls"
          [vd] -> (vd, is)
          _ -> error "newvariants/f: generator with multiple vardecls (not in canonical form)"

partitions :: [a] -> [([a], [a])]
partitions xs = map partitionEithers (mapM (\x -> [Left x, Right x]) xs)

-- showVariant :: (Model, [(VarDecl, Expression)]) -> String
-- showVariant (m, unrollset) =
--   concat [ "variant model:\n"
--          , plainShow m, "\n"
--          , "variant unrollset: ", concat [ concat [showVarDecl vd, " ", showExp e, "  |  "] | (vd,e) <- unrollset ], "\n"
--          ]

showVariant :: Model -> String
showVariant m =
  concat [ "variant model:\n"
         , plainShow m, "\n"
         ]

-- For a given submodel, construct its variant models.
--
-- This involves choosing an unrolling (which is consistent across all
-- constraints) and then replacing the unrolled parts of the forall
-- with a dummy "unrolled" gencall.  This dummy call is used later to
-- assign values to the unrolled variables.
--
-- For example, a submodel with this single constraint:
--   constraint forall(i in 1 .. n,j in 1 .. n where i < j)
--     (x[i] != x[j]);
--
-- has several variants:
-- Variant 1:
--   constraint forall(i in 1 .. n,j in 1 .. n where i < j)
--     (x[i] != x[j]);
-- Variant 2:
--   int: i = LEADER_0;
--   int: j = LEADER_1;
--   constraint unrolled(LEADER_0 in 1 .. n,LEADER_1 in 1.. n)
--                (forall([if i < j then x[i] != x[j] else true endif]));
-- Variant 3:
--   int: i = LEADER_0;
--   constraint unrolled(LEADER_0 in 1 .. n)
--                (forall([forall(j in 1 .. n where i < j) (x[i] != x[j])]));
-- Variant 4:
--   int: j = LEADER_0;
--   constraint unrolled(LEADER_0 in 1 .. n)
--                (forall([forall(i in 1 .. n where i < j) (x[i] != x[j])]));
--
-- Later, we can remove the "unrolled" gencalls and use their
-- generators to determine what values to give to the LEADER_x
-- variables.
variants :: Model -> [Model]
variants sm = do
  let ((ConstraintI c):constraintItems, otherItems) = partition isConstraintI (sm ^. modelItems)
  -- Choose an unrolling of the first constraint.
  (cv, unrollset) <- newvariants c
  -- Unroll all other constraints in the same way.  If any constraint
  -- cannot be unrolled this way, backtrack.
  otherConstraints <- mapM (\ (ConstraintI e) -> matchUnroll unrollset e) constraintItems
  -- Merge the constraint bodies together under a single forall.
  let generatedIdentifiers = [ "LEADER_" ++ show n | n <- [0..] ]
      generatedVarDecls = [ VarDecl parInt ident Nothing mempty Nothing Nothing | ident <- generatedIdentifiers ]
  -- Don't wrap the first body in a let; instead, lift its wrapper to
  -- the top level.
  let newItems = wrapper generatedIdentifiers unrollset
  let bodies = cv : map (uncurry (wrapLet generatedIdentifiers)) otherConstraints
      megaConstraint =
          let e = makeExp (GenCall "unrolled" ( Comprehension { _compGens = [ Generator { _genVarDecls = [vd]
                                                                                  , _genExp = domain
                                                                                  , _genLocation = Nothing } | (vd,domain) <- zip generatedVarDecls (map snd unrollset) ]
                                                        , _compWhere = Nothing
                                                        , _compType = ArrayComprehension
                                                        , _compBody = locExp (Call "forall" [arrayLit bodies]) } ))
          in e
                       
  -- As a special case, if nothing was actually unrolled, we can just
  -- use the original constraints.
  if null unrollset
     then return sm
     else -- Glue it all together.
       return $ ((Model (otherItems ++ newItems ++ [ConstraintI megaConstraint])))
--  return $ ((Model (otherItems ++ [ConstraintI cv] ++ map ConstraintI otherConstraints), context), unrollset)

wrapper :: [String] -> [(VarDecl, Expression)] -> [Item]
wrapper leaderIdents unrollSet =
    [ VarDeclI (vd & varDeclExpression .~ Just (makeExp (Ident leaderident)))
    | (vd, leaderident) <- zip (map fst unrollSet) leaderIdents ]

wrapLet :: [String] -> Expression -> [(VarDecl, Expression)] -> Expression
wrapLet leaderIdents body unrollSet =

    -- makeExp $ Let [ VarDeclI (VarDecl parInt ident (Just (makeExp (Ident leaderident))) mempty mempty)
    --               | (ident, leaderident) <- zip (map unrollSet
    let e = makeExp $ Let (wrapper leaderIdents unrollSet) body
    in e

-- Unroll a constraint in a method that matches the given unroll-set.
-- Return all the ways it might be so unrolled.
matchUnroll :: [(VarDecl, Expression)] -> Expression -> [ (Expression, [(VarDecl, Expression)]) ]
matchUnroll unrollSet e = do
  (e2, unrollSet2) <- newvariants e
  -- XXX: very inefficient method
  unrollSet3 <- permutations unrollSet2
  guard $ matchDomains (map snd unrollSet) (map snd unrollSet3)
  return (e2, unrollSet3)

matchDomains :: [Expression] -> [Expression] -> Bool
matchDomains domain1 domain2 =
    map stripMetadataExp domain1 == map stripMetadataExp domain2

-- Instantiate any dummy "unrolled" calls.  The effect is to assign
-- values to any LEADER_x variables.
instantiations :: Model -> [ Model ]
--instantiations m | trace ("instantiations:\n" ++ plainShow m ++ "\nENDINST") False = undefined
instantiations m = do
    items' <- concat <$> mapM f (m ^. modelItems)
--    let m' = m & modelItems %~ f
    return (m & modelItems .~ items')
  where
    -- The sort of constraint we are targeting is:
    --
    --   constraint unrolled(LEADER_0 in S, LEADER_1 in T) (body);
    --
    -- We eliminate the "unrolled" gencall, leaving only the body.  We
    -- add vardecls for each generator in the gencall, giving values
    -- to each LEADER_x variable.
    --
    -- So the above example would become something like this:
    --   constraint body;
    --   int LEADER_0 = min(S);
    --   int LEADER_1 = max(T);
    --
    -- The choice of min/max is the non-deterministic part.
    f (ConstraintI e) =
        case e ^. expRawExpression of
          GenCall "unrolled" comp ->
              let gens = comp ^. compGens
              in if all targetGen gens
                 then do
                   let vdexppairs = [ (vd,ge) | gen <- gens, let ge = gen ^. genExp, vd <- gen ^. genVarDecls ]
                   vds2 <- forM vdexppairs $ \(vd,e') -> do
                                     which <- [ "min", "max" ]
                                     return (vd & varDeclExpression .~ Just (makeExp (Call which [e'])))
                   return $ ConstraintI (comp ^. compBody)
                            : [ VarDeclI vd | vd <- vds2 ]
                 else return [ConstraintI e]
          _ -> return [ConstraintI e]
    f i = return [i]

targetGen :: Generator -> Bool
targetGen g =
    case g ^. genVarDecls of
      [vd] -> "LEADER_" `isPrefixOf` (vd ^. varDeclIdent)
      _ -> False

getGroups :: Int -> Model -> [([Model],Maybe (Expression))]
getGroups maxConstraints m = do
  (sm,c) <- submodels maxConstraints m
  -- return $! trace ("submodel: " ++ plainShow sm) ()
  v <- variants sm
  -- return $! trace (showVariant v) ()
  return (instantiations v, c)

testSubmodels :: IO ((), Statistics)
testSubmodels = do
  Right m <- parseModelFile "tests/test-unroll.mzn"
  runStatistics $ do
    forM_ (zip [0..] (submodels 2 m)) $ \(n,(sm,_)) -> do
      recordLogKey (T.pack (show n)) (fznShow sm)
    writeLog "submodels.json"

testVariants :: FilePath -> IO ((), Statistics)
testVariants file = do
  Right m <- parseModelFile file
  let m2 = rewriteModel initialNormalisation m
  runStatistics $ do
    recordLogKey "model" (fznShow m2)
    forM_ (zip [0..] (submodels 2 m2)) $ \(n,(sm,c)) -> do
      statisticsTime (T.pack ("submodel " ++ show n)) $ do
        recordLogKey "submodel" (fznShow sm)
        recordLogKey "context" (maybe "-" showExp c)
        forM_ (zip [0..] (variants sm)) $ \ (n2,sm2) -> do
          statisticsTime (T.pack (show n2)) $ do
            recordLogKey "variant" (plainShow sm2)
            forM_ (zip [0..] (instantiations sm2)) $ \ (n3,sm3) -> do
              recordLogKey (T.pack ("instantiation " ++ show n3)) (plainShow sm3)
    writeLog "submodels.json"

main :: IO ((), Statistics)
main = do
  [file] <- getArgs
  testVariants file

showUnrollSet :: [(VarDecl, Expression)] -> String
showUnrollSet us = "{" ++ intercalate ", " (map f us) ++ "}"
  where f (vd,e) = "(" ++ showVarDecl vd ++ ", " ++ showExp e ++ ")"
