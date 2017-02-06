import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import Data.Ord
import Text.Read
import Text.Regex

main = do
  ls <- lines <$> getContents
  let r = mkRegex "^TIME ([^ ]+) (.*)"
  m <- foldM (\acc l ->
               case matchRegex r l of
                 Just [label,numString] -> do
                   let Just n = readMaybe numString
                   return $ M.insertWith (+) label (n::Integer) acc
                 _ -> do putStrLn $ "didn't match line: " ++ l
                         return acc
             ) M.empty ls
  let pairs = M.toList m
  let sorted = sortBy (comparing snd) pairs
  mapM_ (\(l,n) -> putStrLn $ l ++ ":\t " ++ show (fromInteger n / (10^9))) sorted
