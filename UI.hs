import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Tree
import qualified Data.Text as T
import Log
import Language.MiniZinc

import Graphics.UI.Gtk as G

main = do
  l <- either (error "decoding") id . decode <$> BS.readFile "log"
  let t = logToForest l
  -- print (l :: Log)

  initGUI
  w <- windowNew
  w `on` deleteEvent $ (liftIO mainQuit >> return True)
  ts <- treeStoreNew t
  cr <- cellRendererTextNew
  col <- treeViewColumnNew
  G.set col [treeViewColumnTitle := "my column"]
  treeViewColumnPackStart col cr True
  cellLayoutSetAttributes col cr ts $ \r -> [cellText := r]
  tv <- treeViewNewWithModel ts
  treeViewAppendColumn tv col
  treeViewSetHeadersVisible tv True
--  treeViewExpandAll tv

  sw <- scrolledWindowNew Nothing Nothing
  sw `containerAdd` tv
  w `containerAdd` sw

  -- tv `on` buttonPressEvent $ do
  --   liftIO $ do
  --     cs <- treeViewGetColumns tv
  --     print $ length cs
  --   return True

  widgetShowAll w
  mainGUI

logToForest :: Log -> Forest String
logToForest l = map logGroupToTree (l ^.. logGroups . traverse)

logGroupToTree :: LogGroup -> Tree String
logGroupToTree lg = Node "group" $ map logModelToTree (lg ^.. logGroupModels.traverse)

logModelToTree :: LogModel -> Tree String
logModelToTree lm = Node (plainShow (lm ^. logModelModel)) children
  where children = [ Node "solutions" (map (leaf.trim) (lm ^. logModelSolutions))
                   , Node "replacements" (map logReplacementToTree (lm ^.. logModelReplacements.traverse)) ]
                   ++ maybe [] (\mt -> [Node "template" [ leaf (plainShow mt) ]])
                                                 (lm ^. logModelTemplate)

logReplacementToTree :: LogReplacement -> Tree String
logReplacementToTree lr = Node (show (lr ^. logReplacementConstraint) ++ " " ++ show (lr ^. logReplacementArguments) ++ " " ++ show (lr ^. logReplacementSatisfiable)) children
  where children = case lr ^. logReplacementUnsatConstraint of
                     Nothing -> []
                     Just (ConstraintI e) -> [Node (showExp e) []]

leaf = flip Node []

trim = T.unpack . T.strip . T.pack
