import Control.Applicative
import Control.Monad
import Data.Maybe
import Language.MiniZinc
--import Language.MiniZinc.Convert
import System.Environment
import System.IO
import Text.Printf

--import IPPrint

main = do
--  (Just (Model [IncludeI _ (Model items)])) <- readModel "glob.mzn"
  m <- readModel "both-glob.mzn"
--  pprintWidth 180 m
  print m

--   args <- getArgs
--   models <- zip args <$> catMaybes <$> mapM readModel args
--   generalTest models
-- --   testConv models

-- -- testConv :: [(FilePath, Model)] -> IO ()
-- -- testConv models =
-- --   forM_ models $ \(name,model) -> do
-- --                   eb <- testConversion name
-- --                   case eb of
-- --                     Left err -> error err
-- --                     Right False -> error "NON MATCHING ROUNDTRIP!"
-- --                     Right True -> putStrLn "testConversion passed"

-- generalTest models =
--   forM_ models $ \(name,model) -> do
--                   putStrLn name
--                   putStrLn (replicate (length name) '=')
--                   putStrLn ""
--                   putStr =<< prettyPrintModel model
--                   putStrLn ""
--                   pprint model
--                   putStrLn ""
-- --                   model2 <- foldM (\m f -> f m) model (replicate 5 roundTrip)
                  
-- --                   if model == model2
-- --                     then putStrLn "After roundtrips, models are equal."
-- --                     else do
-- --                       putStrLn "After roundtrips, models are NOT equal."
-- --                       let s1 = pshow model
-- --                           s2 = pshow model2
-- --                       printf "The strings compare as %s\n" (if s1 == s2 then "equal" else "unequal")
-- --                       let (d1,d2) = unzip $ dropWhile (uncurry (==)) $ zip s1 s2
-- --                       putStrLn (take 300 d1)
-- --                       putStrLn (take 300 d2)
-- --                       -- pprint model
-- --                       -- pprint model2
-- -- --                  printf "After roundtrip, models are %sequal.\n" (if model==model2 then "" else "NOT ")

readModel :: FilePath -> IO (Maybe (Model Location))
readModel filename = either bad good =<< parseModelFile filename
  where good = return . Just
        bad err = putStrLn (show err) >> return Nothing
