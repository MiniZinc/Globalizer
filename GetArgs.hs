{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GetArgs where

import Data.Data
import Data.Generics.Uniplate.Data
import Language.MiniZinc
import Data.Data.Lens
import qualified Data.Map as M
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Applicative
import qualified Data.Graph.Inductive as G
-- getArguments :: Model () -> [Expression ()]
-- getArguments m = execWriter $ descendBiM f m
--   where f
import Debug.Trace

newtype VarDeclId = VarDeclId Integer
  deriving (Data, Typeable, Ord, Eq, Show)

newtype ReferenceMap a = ReferenceMap (M.Map VarDeclId (VarDecl a))
  deriving (Functor, Data, Typeable, Show)

data Ref a = Ref { _refInner :: a
                 , _refRef :: Maybe VarDeclId
                 }
  deriving (Data, Typeable, Show)

instance Monoid a => Monoid (Ref a) where
    mempty = Ref { _refInner = mempty, _refRef = Nothing }
    mappend x y = Ref { _refInner = (x ^. refInner) `mappend` (y ^. refInner)
                      , _refRef = getFirst (First (x ^. refRef) `mappend` First (y ^. refRef)) }

refRef :: Lens' (Ref a) (Maybe VarDeclId)
refRef = lens _refRef (\r mvdid -> r { _refRef = mvdid })

refInner :: Lens' (Ref a) a
refInner = lens _refInner (\r mvdid -> r { _refInner = mvdid })

-- Add a Nothing annotation to everything.
annotateEmpty :: (Functor f) => f a -> f (Ref a)
annotateEmpty e = over mapped (\a -> Ref a Nothing) e

-- Annotate every VarDecl with a unique identifier.  All other
-- decorations are Nothing.
annotateVarDecls :: forall a f. (Data a, Typeable a, Data (f (Ref a)))
                 => f (Ref a) -> f (Ref a)
annotateVarDecls m =
    let (m',_n) = flip runState 0 $
                    transformMOnOf template template f m
    in m'
  where f :: VarDecl (Ref a) -> State Integer (VarDecl (Ref a))
        f vd = do
          nextNumber <- get
          let vdid = VarDeclId nextNumber
          put (nextNumber+1)
          return $ vd & varDeclDecoration.refRef .~ Just vdid

-- Annotate the identifiers in a model with a reference to their
-- VarDecl.
annotateIdentifiersModel :: (Data a, Typeable a)
                         => Model a -> Model (Ref a)
annotateIdentifiersModel m =
    let m2 = annotateVarDecls . annotateEmpty $ m
        newItems =
          runIdentity $
            flip evalStateT M.empty $
              mapM annotateIdentifiersItem' (m2 ^. modelItems)
    in m & modelItems .~ newItems

updateMap :: [VarDecl (Ref a)] -> M.Map VarId VarDeclId -> M.Map VarId VarDeclId
updateMap vds m = foldl' (flip updateMap1) m vds
updateMap1 vd = M.insert
                    (vd ^. varDeclIdent)
                    (fromMaybe (error "vardecl with no annotation") (vd ^. varDeclDecoration.refRef))

annotateIdentifiersItem' :: (Typeable a, Data a)
       => Item (Ref a)
       -> StateT (M.Map VarId VarDeclId) Identity (Item (Ref a))
annotateIdentifiersItem' (VarDeclI vd) = do
  currentBindings <- get
  newVD <- lift $ flip runReaderT currentBindings $ annotateIdentifiers vd
  modify $ updateMap [newVD]
  return $ VarDeclI newVD
annotateIdentifiersItem' i = do
  currentBindings <- get
  newI <- lift $ flip runReaderT currentBindings $ annotateIdentifiers i
  return $ newI

annotateIdentifiers :: forall a f. (Typeable a, Data a, Functor f, Data (f (Ref a)))
          => f (Ref a) -> ReaderT (M.Map VarId VarDeclId) Identity (f (Ref a))
annotateIdentifiers e = descendBiM f e
  where
    handleComprehension (Comprehension body [] whereExp compType) = do
      newBody <- f body
      newWhere <- descendBiM f whereExp
      return $ Comprehension newBody [] newWhere compType
    handleComprehension (Comprehension body (gen:gens) whereExp compType) = do
      newGen@(Generator {_genVarDecls=vds}) <- descendBiM f gen
      let makeNewEnv = updateMap vds
      recurse <- local makeNewEnv (handleComprehension (Comprehension body gens whereExp compType))
      return $ recurse & compGens %~ (newGen:)
    f :: Expression (Ref a) -> ReaderT (M.Map VarId VarDeclId) Identity (Expression (Ref a))
    f e =
      case e ^. expRawExpression of
        (Let vds body) -> do
          newVDs <- descendBiM f vds
          newBody <- local (updateMap vds) (f body)
          return $ e { _expRawExpression = Let newVDs newBody }
        (ComprehensionExpr c) -> do
          newC <- handleComprehension c
          return $ e & expRawExpression .~ ComprehensionExpr newC
        (GenCall f c) -> do
          newC <- handleComprehension c
          return $ e & expRawExpression .~ GenCall f newC
        (Ident y) -> do
          env <- ask
          return $ e & expDecoration.refRef .~ M.lookup y env
        y -> do e' <- descendBiM f y
                return $ e & expRawExpression .~ e'

buildReferenceMap :: forall a. (Data a, Typeable a)
                 => Model (Ref a) -> ReferenceMap (Ref a)
buildReferenceMap m =
    flip execState (ReferenceMap M.empty) $
      traverseOf_ template f m
  where f :: VarDecl (Ref a) -> State (ReferenceMap (Ref a)) ()
        f vd = do
          -- First, descend.
          traverseOf_ template f vd
          -- Next, add this vardecl to the reference map.
          ReferenceMap rm <- get
          let msg = "var decl has not been annotated"
          let vdid = fromMaybe (error msg) $ vd ^. varDeclDecoration.refRef
          put (ReferenceMap (M.insert vdid vd rm))

suffixIdentifiers :: forall a. (Data a, Typeable a)
                  => (Model (Ref a), ReferenceMap (Ref a))
                  -> (Model (Ref a), ReferenceMap (Ref a))
suffixIdentifiers = transformOnOf template template f
                    . transformOnOf template template g
  where f :: Expression (Ref a) -> Expression (Ref a)
        f e = fromMaybe e $ do
                Ident vid <- return $ e ^. expRawExpression
                let (VarDeclId vdid) = fromMaybe (error $ vid) $ e ^. expDecoration.refRef
                return $ e & expRawExpression .~ Ident (vid ++ "_" ++ show vdid)
        g :: VarDecl (Ref a) -> VarDecl (Ref a)
        g vd = fromMaybe vd $ do
                 Just (VarDeclId vdid) <- return $ vd ^. varDeclDecoration.refRef
                 return $ vd & varDeclIdent <>~ ("_" ++ show vdid)

externalItemDefines :: forall a. (Data a, Typeable a) => Item (Ref a) -> [VarDeclId]
externalItemDefines (VarDeclI vd) = [fromJust $ vd ^. varDeclDecoration.refRef]
externalItemDefines _i = []

itemDefines :: forall a. (Data a, Typeable a) => Item (Ref a) -> [VarDeclId]
itemDefines i = catMaybes (map f (universeOnOf template template i))
  where f :: VarDecl (Ref a) -> Maybe VarDeclId
        f vd = vd ^. varDeclDecoration.refRef

itemDependencies :: forall a. (Data a, Typeable a) => Item (Ref a) -> [VarDeclId]
itemDependencies i = catMaybes (map f (universeOnOf template template i))
  where f :: Expression (Ref a) -> Maybe VarDeclId
        f e = do Ident {} <- return $ e ^. expRawExpression
                 e ^. expDecoration.refRef

externalItemDependencies :: forall a. (Data a, Typeable a) => Item (Ref a) -> [VarDeclId]
externalItemDependencies i = itemDependencies i \\ itemDefines i


testAnnotation :: FilePath -> IO ()
testAnnotation path = do
    inputModel <- readModel <$> readFile path
    putStrLn $ plainShow $ inputModel
    putStrLn ""
    let sortedModel = inputModel & modelItems %~ sortItemsByDependencyIds
    putStrLn $ plainShow $ sortedModel
    putStrLn ""
    let m = annotateIdentifiersModel $ sortedModel
--    let m = m' & modelItems %~ 
    let rm = buildReferenceMap m
    let (m3,rm3) = suffixIdentifiers (m,rm)
    putStrLn . plainShow $ m3
    let ReferenceMap rminner = rm3
    mapM_ print (M.keys rminner)
    mapM_ (print.itemDefines) (m3 ^. modelItems)
    putStrLn ""
    mapM_ (print.itemDependencies) (m3 ^. modelItems)
    putStrLn ""
    mapM_ (print.externalItemDefines) (m3 ^. modelItems)
    putStrLn ""
    mapM_ (print.externalItemDependencies) (m3 ^. modelItems)

sortItemsByDependency :: (Data a, Typeable a) => [Item (Ref a)] -> [Item (Ref a)]
sortItemsByDependency items =
    let itemMap = M.fromList (zip [0..] items)
        itemNumbers = M.keys itemMap
        deps = M.map externalItemDependencies itemMap
        defs = M.map externalItemDefines itemMap
        flatten (x,ys) = [ (x,y) | y <- ys ]
        invdefs = M.fromList . map (\(x,y) -> (y,x)) . concatMap flatten . M.toList $ defs
        nodes = (zip [0..] items)
        edges = [ (a,b) | (a,ai) <- nodes,
                          let adeps = deps M.! a,
                          vdid <- adeps,
                          let b = invdefs M.! vdid ]
        gr = G.mkUGraph itemNumbers edges :: G.Gr () ()
        ordering = G.topsort gr
    in map (\n -> itemMap M.! n) ordering

sortItemsByDependencyIds :: (Data a, Typeable a) => [Item a] -> [Item a]
sortItemsByDependencyIds items =
    let itemMap = M.fromList (zip [0..] items)
        itemNumbers = M.keys itemMap
        deps = M.map freeIdentifiers itemMap
        defs = M.fromList [ (k,v) | (k,i) <- M.toList itemMap,
                                    let mvid = itemDefinedId i,
                                    isJust mvid,
                                    let v = fromJust mvid ]
        invdefs = M.fromList . map (\(x,y) -> (y,x)) . M.toList $ defs
        nodes = (zip [0..] items)
        edges = [ (b,a) | (a,ai) <- nodes,
                          let adeps = deps M.! a,
                          vid <- adeps,
                          let b = invdefs M.! vid ]
        gr = G.mkUGraph itemNumbers edges :: G.Gr () ()
        ordering = G.topsort gr
    in map (\n -> itemMap M.! n) ordering

itemDefinedId :: Item a -> Maybe VarId
itemDefinedId (VarDeclI vd) = Just $ vd ^. varDeclIdent
itemDefinedId _ = Nothing

readModel :: String -> Model Location
readModel = either (error.show) id . parseString model

freeIdentifiers :: forall a. (Data a, Typeable a) => Item a -> [VarId]
freeIdentifiers i =
    let i' = flip evalState M.empty $
               annotateIdentifiersItem' . annotateVarDecls . annotateEmpty $ i
        -- Any unannotated identifiers in i' are free.
        f :: Expression (Ref a) -> Maybe VarId
        f e = do Ident x <- return $ e ^. expRawExpression
                 case e ^. expDecoration.refRef of
                   Just v -> Nothing
                   Nothing -> Just x
    in catMaybes $ map f $ universeOnOf template template i'
