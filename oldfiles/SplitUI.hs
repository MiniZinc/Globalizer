import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.List
import Data.Serialize
import Data.Tree
import qualified Data.Text as T
import Log
import Language.MiniZinc
import Language.MiniZinc.Analysis
import Data.Data

import qualified Data.Graph.Inductive as Gr

import Graphics.UI.Gtk as G

main = do
  initGUI
  w <- windowNew
  w `on` deleteEvent $ (liftIO mainQuit >> return True)
  ts <- listStoreNew []
  cr <- cellRendererTextNew
  col <- treeViewColumnNew
  G.set col [treeViewColumnTitle := "my column"]
  treeViewColumnPackStart col cr True
  cellLayoutSetAttributes col cr ts $ \r -> [cellTextMarkup := Just $ if connected r then escapeMarkup (plainShow r) else "<b>not connected</b>"]
  tv <- treeViewNewWithModel ts
  treeViewAppendColumn tv col
  treeViewSetHeadersVisible tv True
--  treeViewExpandAll tv

  sw <- scrolledWindowNew Nothing Nothing
  sw `containerAdd` tv
  hbox <- hBoxNew True 5
  input <- textViewNew
  inputBuf <- textViewGetBuffer input
  output <- textViewNew
  outputBuf <- textViewGetBuffer output
  messages <- textViewNew
  messagesBuf <- textViewGetBuffer messages
  vbox <- vBoxNew True 5
  boxPackStart vbox sw PackGrow 0
  boxPackStart vbox messages PackGrow 0
  boxPackStart hbox input PackGrow 0
  boxPackStart hbox vbox PackGrow 0
  boxPackStart hbox output PackGrow 0
  w `containerAdd` hbox

  -- tv `on` buttonPressEvent $ do
  --   liftIO $ do
  --     cs <- treeViewGetColumns tv
  --     print $ length cs
  --   return True

  inputBuf `on` bufferChanged $ do
    startIter <- textBufferGetStartIter inputBuf
    endIter <- textBufferGetEndIter inputBuf
    contents <- textBufferGetByteString inputBuf startIter endIter True
    case parseModel contents "input" of
      Left pe -> do textBufferSetText messagesBuf (show pe)
      Right m -> do let m' = evalModel m
                    textBufferSetText messagesBuf (plainShow m')
                    let subs = splitModel m'
                        subs' = filter connected subs
                    print (length subs)
                    listStoreClear ts
                    mapM_ (listStoreAppend ts) subs

  widgetShowAll w
  mainGUI

connected :: (Data a, Typeable a, Eq a, Show a) => Model a -> Bool
connected m =
    not (null (filter isConstraintI (m ^. modelItems))) &&
    let constraints = filter isConstraintI (m ^. modelItems)
        nodes = zip [1..] constraints
        edges = [ (n1,n2,()) | (n1,c1) <- nodes, (n2,c2) <- nodes, hasEdge c1 c2 ]
        hasEdge c1 c2 = not (null (intersect (freeIdentifiers c1) (freeIdentifiers c2)))
        connectionGraph = mkGr nodes edges
        mkGr :: [Gr.LNode n] -> [Gr.LEdge e] -> Gr.Gr n e
        mkGr = Gr.mkGraph
    in Gr.isConnected connectionGraph

splitModel :: Model a -> [Model a]
splitModel m = do
  newItems <- concat <$> mapM decide (m ^. modelItems)
  return $ m & modelItems .~ newItems

decide :: Item a -> [[Item a]]
decide (i@(ConstraintI{})) = [ [], [i] ]
decide i = [[i]]

-- logToForest :: Log -> Forest String
-- logToForest l = map logGroupToTree (l ^.. logGroups . traverse)

-- logGroupToTree :: LogGroup -> Tree String
-- logGroupToTree lg = Node "group" $ map logModelToTree (lg ^.. logGroupModels.traverse)

-- logModelToTree :: LogModel -> Tree String
-- logModelToTree lm = Node (plainShow (lm ^. logModelModel)) children
--   where children = [ Node "solutions" (map (leaf.trim) (lm ^. logModelSolutions))
--                    , Node "replacements" (map logReplacementToTree (lm ^.. logModelReplacements.traverse))
--                    ]

-- logReplacementToTree :: LogReplacement -> Tree String
-- logReplacementToTree lr = Node (show (lr ^. logReplacementConstraint) ++ " " ++ show (lr ^. logReplacementArguments) ++ " " ++ show (lr ^. logReplacementSatisfiable)) children
--   where children = case lr ^. logReplacementUnsatConstraint of
--                      Nothing -> []
--                      Just (ConstraintI e) -> [Node (showExp e) []]

-- leaf = flip Node []

-- trim = T.unpack . T.strip . T.pack
