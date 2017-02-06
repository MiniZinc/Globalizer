import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Language.MiniZinc
import Language.MiniZinc.Convert
import System.Environment
import System.IO

import GroupOutput
import Statistics

main = flip execStateT emptyStatistics $ do
                files <- liftIO $ getArgs

                models <- liftIO $ forM files $ \a -> do
                            mm <- readModelFile a
                            case mm of
                              Left err -> error ("error reading: " ++ err)
                              Right m -> return m

                -- mm <- readModelFile =<< (ehead "5" <$> getArgs)
                -- let m = case mm of
                --           Left err -> error 
                --           Right m' -> m'

                -- putStrLn ""
                -- print (outputExp m)
                -- putStrLn ""
                -- mapM_ print (groupVariables (variables (outputExp m)))
                -- putStrLn ""
                -- mapM_ print (map groupIdentifier (groupVariables (variables (outputExp m))))
                -- putStrLn ""

                let m = head models

                inter <- statisticsTime "finding intersection" $ do
                  replacementList <- fromJust <$> foldM (\reps m -> do
                                                           logLn "starting model"
                                                           Just <$> processModel m (reps :: Maybe [(Replacement,[ConstraintNumber],Double)]))
                                                        Nothing
                                                        models
              --    let inter' = foldr1 intersect replacementList
                  let inter' = replacementList
                  logLn "intersection is:"
                  mapM_ logPrint inter'
                  logLn "done"

                  let inter = tightest (topLevelBindings m) inter'
                  logLn "non-dominated intersection is:"
                  mapM_ logPrint inter
                  logLn "done"
                  return inter
                
                let rockSolidConstraints = inter
                liftIO $ putStrLn "OUTPUT"
                liftIO $ forM_ rockSolidConstraints $ \(rep,ctx,scr) -> do
                  putStr (show scr)
                  putStr " "
                  putStr (show ctx)
                  putStr " "
                  putStrLn (prettyPrintify m rep)
