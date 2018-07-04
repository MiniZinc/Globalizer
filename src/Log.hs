{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}

module Log where

import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Serialize
import GHC.Generics
import Language.MiniZinc

import Types

data LogState = LogState { _logStateLog :: Log
                         , _logStateCursor :: LogCursor }
  deriving (Show, Read, Generic)

data LogCursor = LogCursor { _logCursorGroup :: Maybe Int
                           , _logCursorModel :: Maybe Int }
  deriving (Show, Read, Generic)

data Log = Log { _logGroups :: Seq LogGroup }
  deriving (Show, Read, Generic)

data LogGroup = LogGroup { _logGroupModels :: Seq LogModel }
  deriving (Show, Read, Generic)

data LogModel = LogModel { _logModelModel :: Model ()
                         , _logModelReplacements :: Seq LogReplacement
                         , _logModelSolutions :: [String]
                         , _logModelTemplate :: Maybe (Model ()) }
  deriving (Show, Read, Generic)

data LogReplacement = LogReplacement { _logReplacementConstraint :: Constraint
                                     , _logReplacementModel :: Model ()
                                     , _logReplacementArguments :: [Argument]
                                     , _logReplacementSatisfiable :: Bool
                                     , _logReplacementUnsatConstraint :: Maybe (Item ())
                                     }
  deriving (Show, Read, Generic)

instance Serialize Log
instance Serialize LogGroup
instance Serialize LogModel
instance Serialize LogReplacement

makeLenses ''LogState
makeLenses ''LogCursor
makeLenses ''Log
makeLenses ''LogGroup
makeLenses ''LogModel
makeLenses ''LogReplacement

emptyLogState = LogState { _logStateLog = Log { _logGroups = Seq.empty }
                         , _logStateCursor = LogCursor { _logCursorGroup = Nothing
                                                       , _logCursorModel = Nothing }
                         }

logRecordReplacement :: Monad m => Replacement -> Model () -> Bool -> Maybe (Item ()) -> StateT LogState m ()
logRecordReplacement r model b muc = modify $ \ls ->
    case getCursor ls of
      Nothing -> error "logRecordReplacement: invalid cursor"
      Just (g,m) ->
          ls & logStateLog
             . logGroups . ix g
             . logGroupModels . ix m
             . logModelReplacements %~ (|> lr)
  where
    lr = LogReplacement { _logReplacementConstraint = r ^. _1
                        , _logReplacementArguments = r ^. _2
                        , _logReplacementModel = model
                        , _logReplacementSatisfiable = b
                        , _logReplacementUnsatConstraint = muc }

logRecordTemplate :: Monad m => Model () -> StateT LogState m ()
logRecordTemplate model = modify $ \ls ->
    case getCursor ls of
      Nothing -> error "logRecordReplacement: invalid cursor"
      Just (g,m) ->
          ls & logStateLog
             . logGroups . ix g
             . logGroupModels . ix m
             . logModelTemplate .~ Just model
               

logNewGroup :: Monad m => StateT LogState m ()
logNewGroup = modify $ \ls ->
              let ngroups = Seq.length (ls ^. logStateLog . logGroups)
              in ls & logStateCursor . logCursorGroup .~ Just ngroups
                    & logStateLog . logGroups %~ (|> lg)
  where lg = LogGroup { _logGroupModels = Seq.empty }

logNewModel :: Monad m => Model () -> [String] -> StateT LogState m ()
logNewModel m sols = modify $ \ls ->
                     let modelsLens = logStateLog
                                      . logGroups . ix (getGroupCursor ls)
                                      . logGroupModels
                         nmodels = Seq.length (ls ^. modelsLens)
                     in ls & logStateCursor . logCursorModel .~ Just nmodels
                           & modelsLens %~ (|> lm)
  where
    getGroupCursor ls = fromMaybe (error "logNewModel: invalid cursor")
                          (ls ^. logStateCursor ^. logCursorGroup)
    lm = LogModel { _logModelModel = m
                  , _logModelReplacements = Seq.empty
                  , _logModelSolutions = sols
                  , _logModelTemplate = Nothing }

logModelAttachSolutions :: Monad m => [String] -> StateT LogState m ()
logModelAttachSolutions sols = modify $ \ls ->
    case getCursor ls of
      Nothing -> error "logModelAttachSolutions: invalid cursor"
      Just (g,m) ->
          ls & logStateLog
             . logGroups . ix g
             . logGroupModels . ix m
             . logModelSolutions %~ (++ sols)

getCursor ls = do g <- ls ^. logStateCursor ^. logCursorGroup
                  m <- ls ^. logStateCursor ^. logCursorModel
                  return (g,m)

increment Nothing = Just 0
increment (Just x) = Just (x+1)


combineLogs :: [Log] -> Log
combineLogs logs = Log { _logGroups = foldr1 (Seq.><) (logs ^.. traverse . logGroups) }
