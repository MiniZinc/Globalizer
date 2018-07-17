{-# OPTIONS_GHC -Wall #-}

module Arguments where

import Bindings
import Common
import MiscExp
import SimpleLog
import Statistics
import Types

import Control.Lens
import Control.Monad
import Data.Data.Lens
import Data.List
import Data.Maybe
import Language.MiniZinc

computePotentialArguments :: Bindings
                          -> [VarId]
                          -> [VarDecl]
                          -> Expression
                          -> Bool
                          -> ChannelMap
                          -> SimpleLog.Handle
                          -> StatisticsIO [Argument]
computePotentialArguments templateEnv identifiersInSolution parVarDecls allConstraintsExpression filterArguments channelMap logHandle = do
  -- The potential arguments are:
  --   * the original parameters (e.g. "n")
  --   * original variables, which are now solutions (e.g. "x")

  -- Take all the possible arguments...
  let potentialArguments1 =   []
                           ++ [ ErstwhileVariable vid                          | vid <- identifiersInSolution ]
                           ++ [ OrdinaryParameter (Ident (vd ^. varDeclIdent)) | vd <- parVarDecls ]
  -- ... but keep only the ones that appear in some constraint in the
  -- submodel (or the context).
  let interestingExpressions = map (view expRawExpression) (universeOf template allConstraintsExpression)
  let potentialArguments1Filtered =
          if filterArguments
          then filter (\a -> fromMaybe False $ do
                               i <- argumentToIdent a
                               return $ Ident i `elem` interestingExpressions) potentialArguments1
          else potentialArguments1

  forM_ potentialArguments1Filtered $ \a -> do
     Just aid <- return $ argumentToIdent a
     SimpleLog.log logHandle LogArgs $ "argumentOccurences (" ++ aid ++ "/1): " ++ show (argumentOccurrences templateEnv allConstraintsExpression aid 1)
     SimpleLog.log logHandle LogArgs $ "argumentOccurences (" ++ aid ++ "/2): " ++ show (argumentOccurrences templateEnv allConstraintsExpression aid 2)
  
  let check idx aid n = 
          let Ident idxident = idx
          in not ("LEADER_" `isPrefixOf` idxident)
             && or [ idxident == "_"
                   , idxident `elem` argumentOccurrences templateEnv allConstraintsExpression aid n
                   , case find (\(_b,x,_idxmap) -> x == aid) channelMap of
                       Just (b,_x,idxmap) ->
                         idxident `elem` argumentOccurrences templateEnv allConstraintsExpression b (idxmap !! (n-1))
                       Nothing -> False
                   ]

  let potentialArguments = potentialArguments1Filtered
                           ++ [ ArgumentArrayAccess a [idx1,idx2,idx3]
                              | a <- potentialArguments1Filtered,
                                is3DArray templateEnv a,
                                Just aid <- return $ argumentToIdent a,
                                OrdinaryParameter idx1 <- OrdinaryParameter (Ident "_") : potentialArguments1Filtered,
                                check idx1 aid 1,
                                OrdinaryParameter idx2 <- OrdinaryParameter (Ident "_") : potentialArguments1Filtered,
                                check idx2 aid 2,
                                OrdinaryParameter idx3 <- OrdinaryParameter (Ident "_") : potentialArguments1Filtered,
                                check idx3 aid 3,
                                not (idx1 == Ident "_" && idx2 == Ident "_" && idx3 == Ident "_")
                              ]
                           ++ [ ArgumentArrayAccess a [idx1,idx2]
                              | a <- potentialArguments1Filtered,
                                is2DArray templateEnv a,
                                Just aid <- return $ argumentToIdent a,
                                OrdinaryParameter idx1 <- OrdinaryParameter (Ident "_") : potentialArguments1Filtered,
                                check idx1 aid 1,
                                OrdinaryParameter idx2 <- OrdinaryParameter (Ident "_") : potentialArguments1Filtered,
                                check idx2 aid 2,
                                not (idx1 == Ident "_" && idx2 == Ident "_")
                              ]
                           ++ [ ArgumentArrayAccess a [idx]
                              | a <- potentialArguments1Filtered,
                                is1DArray templateEnv a,
                                Just aid <- return $ argumentToIdent a,
                                OrdinaryParameter idx <- potentialArguments1Filtered,
                                check idx aid 1
                              ]
                           ++ [ OrdinaryParameter (IntLit 0) ]
  return potentialArguments
