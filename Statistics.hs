{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Statistics where

import Control.Lens
import Control.Monad.State.Strict
import Data.Aeson as A
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as B
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Monoid
import Data.Time
import Text.Printf

type Key = [T.Text]

data Statistics = Statistics {
      _numberModelEvaluations :: !Integer
    , _numberFlatZincCalls :: !Integer
    , _numberSuccessfulImpliesChecks :: !Integer
    , _labelledTime :: M.Map Key Double
    , _currentKey :: !Key

    , _logTree :: M.Map Key T.Text
    }
  deriving (Show)

data LogEntry = Subtree NestedLog
              | Leaf T.Text
  deriving (Show)

type NestedLog = M.Map T.Text LogEntry

instance ToJSON LogEntry where
  toJSON = logEntryToJSON ""

logEntryToJSON :: T.Text -> LogEntry -> Value
logEntryToJSON key (Leaf t) = object [ "text" A..= String key
                                     , "extra" A..= String t ]
logEntryToJSON key (Subtree m) =
    object [ "text" A..= String key
           , "children" A..= Array (V.fromList [ logEntryToJSON k v | (k,v) <- M.toList m ])
           ]

nestedLogToJSON :: NestedLog -> Value
nestedLogToJSON = logEntryToJSON "" . Subtree


logTreeToNestedLog :: M.Map Key T.Text -> NestedLog
logTreeToNestedLog t = foldl' ins M.empty (map (_1 %~ reverse) (M.toList t))

ins :: NestedLog -> ([T.Text], T.Text) -> NestedLog
ins m ([],v) = M.insertWith (\(Leaf new) (Leaf old) -> Leaf (T.concat [old, T.pack "\n", new])) (T.pack "val") (Leaf v) m
ins m ([k],v) = M.insertWith merge k (Leaf v) m
  where merge (Leaf new) (Leaf old) = Leaf (T.concat [old, T.pack "\n", new])
        merge (Subtree _) (Subtree _) = error $ show k
        merge _ _ = error $ show k
ins m ((k:ks),v) = let Subtree subm = M.findWithDefault (Subtree M.empty) k m
                       subm' = ins subm (ks,v)
                   in M.insert k (Subtree subm') m

instance Monoid Statistics where
    mempty = emptyStatistics
    mappend s1 s2 = --emptyStatistics
        Statistics { _numberModelEvaluations = _numberModelEvaluations s1 + _numberModelEvaluations s2
                   , _numberFlatZincCalls = _numberFlatZincCalls s1 + _numberFlatZincCalls s2
                   , _numberSuccessfulImpliesChecks = _numberSuccessfulImpliesChecks s1 + _numberSuccessfulImpliesChecks s2
                   , _labelledTime = M.unionWith (+) (_labelledTime s1) (_labelledTime s2)
                   , _logTree = M.unionWith T.append (_logTree s1) (_logTree s2)
                   , _currentKey = _currentKey s1 }

emptyStatistics :: Statistics
emptyStatistics = Statistics { _numberModelEvaluations = 0
                             , _numberFlatZincCalls = 0
                             , _numberSuccessfulImpliesChecks = 0
                             , _labelledTime = M.empty
                             , _logTree = M.empty
                             , _currentKey = [] }

runStatistics = flip runStateT emptyStatistics

-- Merge externally gathered statistics into the current statistics set.
mergeStatistics :: Statistics -> StatisticsIO ()
mergeStatistics extStats = 
  -- trace ("mergeStatistics: " ++ show extStats) $
    modify (<> extStats)

writeLog filename = do
  s <- get
  liftIO $ B.writeFile filename $ encodePretty $ nestedLogToJSON $ logTreeToNestedLog $ s ^. logTree

numberModelEvaluations f s = fmap (\x -> s { _numberModelEvaluations = x }) (f (_numberModelEvaluations s))
numberFlatZincCalls f s = fmap (\x -> s { _numberFlatZincCalls = x }) (f (_numberFlatZincCalls s))
numberSuccessfulImpliesChecks f s = fmap (\x -> s { _numberSuccessfulImpliesChecks = x }) (f (_numberSuccessfulImpliesChecks s))
labelledTime f s = fmap (\x -> s { _labelledTime = x }) (f (_labelledTime s))
currentKey f s = fmap (\x -> s { _currentKey = x }) (f (_currentKey s))
logTree f s = fmap (\x -> s { _logTree = x }) (f (_logTree s))

type StatisticsIO = StateT Statistics IO

statisticsEvaluation :: StatisticsIO ()
statisticsEvaluation = numberModelEvaluations += 1

statisticsEvaluationAdd :: Integral i => i -> StatisticsIO ()
statisticsEvaluationAdd x = numberModelEvaluations += fromIntegral x

statisticsFlatZincCall :: StatisticsIO ()
statisticsFlatZincCall = numberFlatZincCalls += 1

statisticsFlatZincCallAdd :: Integral i => i -> StatisticsIO ()
statisticsFlatZincCallAdd x = numberFlatZincCalls += fromIntegral x

statisticsSuccessfulImpliesCheck :: StatisticsIO ()
statisticsSuccessfulImpliesCheck = numberSuccessfulImpliesChecks += 1

pushString :: T.Text -> StatisticsIO ()
pushString s = currentKey %= (s:)

popString :: StatisticsIO ()
popString = currentKey %= tail

showStatistics :: Statistics -> String
showStatistics stats =
  let statPairs = M.toList $ stats ^. labelledTime
      flippedPairs = map (over _1 reverse) statPairs
  in showPairs 4 flippedPairs

showPairs :: Int -> [(Key,Double)] -> String
showPairs offset pairs = do
  let basePairs = filter ((==1) . length . fst) pairs
  flip concatMap basePairs $ \([k],v) ->
    (concat [ replicate offset ' '
            , T.unpack k
            , " "
            , printf "%.1f" v
            , "\n" ])
    ++ showPairs (offset+4) (filter (not . null . fst) . map (\(k',v') -> (tail k',v')) . filter ((==k) . head . fst) $ pairs)

statisticsTime :: T.Text -> StatisticsIO a -> StatisticsIO a
-- statisticsTime label action = ... 
statisticsTime _ action = action

time' action = do
  t1 <- liftIO $ getCurrentTime
  a <- action
  t2 <- liftIO $ getCurrentTime
  let diff = diffUTCTime t2 t1
  return (a, realToFrac diff)

-- s = Statistics {_numberModelEvaluations = 4765, _numberFlatZincCalls = 516, _numberSuccessfulImpliesChecks = 0, _labelledTime = M.fromList [(["finding intersection","whole program"],35400),(["getGoodConstraints","finding intersection","whole program"],30988),(["pretty printing minizinc","solving via minizinc","finding intersection","whole program"],13),(["pretty printing minizinc","solving via minizinc","getGoodConstraints","finding intersection","whole program"],8),(["solving via minizinc","finding intersection","whole program"],1080),(["solving via minizinc","getGoodConstraints","finding intersection","whole program"],16306),(["whole program"],35542)], _currentKey = []}

logging = False

recordLog :: String -> StatisticsIO ()
recordLog msg = when logging $ do
    key <- use currentKey
    logTree %= (at key %~ (Just . (\existing -> T.concat [existing, T.pack "\n", T.pack msg]) . fromMaybe (T.pack "")))

recordLogKey :: T.Text -> String -> StatisticsIO ()
recordLogKey k msg = when logging $ do
    key <- use currentKey
    logTree %= (at (k:key) %~ (Just . (\existing -> T.concat [existing, T.pack "\n", T.pack msg]) . fromMaybe (T.pack "")))
