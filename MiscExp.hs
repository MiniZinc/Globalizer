module MiscExp where

import Control.Lens
import Control.Monad
import Data.Data.Lens
import Data.Generics.Uniplate.Data as U
import Data.List
import Language.MiniZinc

argumentOccurrences' :: Expression -> VarId -> Int -> [VarId]
argumentOccurrences' e a arg =
    [ i | aa@(ArrayAccess ae args) <- U.universeBi e :: [Expression'],
          Ident ai <- return (ae ^. expRawExpression),
          ai == a,
          length args >= arg,
          Ident i <- return (args !! (arg-1) ^. expRawExpression)
          ]

argumentOccurrences :: Bindings -> Expression -> VarId -> Int -> [VarId]
argumentOccurrences env e a arg =
  let plainOccurrences = argumentOccurrences' e a arg
      e' = replaceWithIntegerVersions env e
      integerVersions = argumentOccurrences' e' a arg
  in plainOccurrences ++ integerVersions

replaceWithIntegerVersions :: Bindings -> Expression -> Expression
replaceWithIntegerVersions env = transformOf template f
  where f e = case integerVersion env (e ^. expRawExpression) of
                Nothing -> e
                Just e' -> makeExp e'

integerVersion :: Bindings -> Expression' -> Maybe Expression'
integerVersion env e' = do
  ArrayAccess ae args <- return e'
  Ident ai <- return (ae ^. expRawExpression)
  vd <- lookupVarDecl ai env
  let Annotations es = vd ^. varDeclAnnotations
  (x,idx) <- msum (map getBooleanises es)
--  Call "booleanises" (Ident x) <- find isBooleanises es
  return $ ArrayAccess (makeExp $ Ident x) (dropIndex (idx-1) args)

dropIndex :: Integral i => i -> [a] -> [a]
dropIndex i xs = genericTake i xs ++ genericDrop (i+1) xs

getBooleanises :: Monad m => Expression -> m (String, Integer)
getBooleanises e = do
  Call "booleanises" [x,idxe] <- return $ e ^. expRawExpression
  Ident i <- return $ x ^. expRawExpression
  IntLit idx <- return $ idxe ^. expRawExpression
  return (i,idx)

test1 :: Bool
test1 = argumentOccurrences' (stripMetadataExp $ parseExp "x[i]+x[j,k]") "x" 1
        == ["i", "j"]

test2 :: Bool
test2 = argumentOccurrences' (evalExpIdentifiers $ stripMetadataExp $ parseExp "let {int:i=j} in x[i]") "x" 1
        == ["j"]

test3 :: Bool
test3 = argumentOccurrences' (evalExpIdentifiers $ stripMetadataExp $ parseExp " forall(i in (1) .. (9),k in (1) .. (9),j in (1) .. (9)) (((p[i, j]) = (k)) <-> ((x[i, j, k]) = (1)))") "p" 2
        == ["j"]

modelString :: String
modelString = unlines $
    [ "annotation booleanises(array[int,int] of var int : x);"
    , "int : n=3;"
    , "int : m=4;"
    , ""
    , "array[1..n, 1..n, 1..m] of var 0..1 : b :: booleanises(x);"
    , "array[1..n, 1..n] of var 1..m : x;"
    , ""
    , "% channeling"
    , "constraint forall (aa,bb in 1..n, cc in 1..m) (x[aa,bb]=cc <-> b[aa,bb,cc]=1);"
    , ""
    , "constraint forall (i1 in 1..n)"
    , "  ( forall (v in 1..m)"
    , "    ( sum([b[i1,i2,v] | i2 in 1..n]) <= 1 ));"
    , ""
    , "solve satisfy;" ]

test4 :: Bool
test4 =
    let (Right m') = parseString model modelString
        m = m'
        constraintExpressions = [ e | ConstraintI e <- m ^. modelItems ]
        allConstraintsExpression = evalExpIdentifiers $ makeExp $ Call "?" constraintExpressions
    in nub (argumentOccurrences (topLevelBindings m) allConstraintsExpression "x" 2)
       == ["bb", "i2"]

testE :: Expression
testE = parseExp "(let {int: i1 = LEADER_i1} in (forall(v in V) (b[i1, i2, v])))"

test5 :: Expression
test5 = evalExpIdentifiers (parseExp "let {int:i=j} in x[i]")
