{-

35:5..35:11 35:18..35:22 36:9..36:67 / 43:5..43:11 43:32..43:71 44:9..45:38 /
30 [ConstraintNumber 0] sliding_sum(0,cars_in_class[p],option_block_size[p],[step_option_use[1,5],step_option_use[2,5],step_option_use[3,5],step_option_use[4,5],step_option_use[5,5],step_option_use[6,5],step_option_use[7,5],step_option_use[8,5],step_option_use[9,5],step_option_use[10,5]])



35:5..35:11 35:18..35:22 36:9..36:67 / 43:5..43:11 43:32..43:71 44:9..45:38 /
30 [ConstraintNumber 0] sliding_sum(0,cars_in_class[p],option_block_size[p],[step_option_use[1,5],step_option_use[2,5],step_option_use[3,5],step_option_use[4,5],step_option_use[5,5],step_option_use[6,5],step_option_use[7,5],step_option_use[8,5],step_option_use[9,5],step_option_use[10,5]])
35:5..35:11 35:30..35:36 35:18..35:22 36:9..36:67 / 51:5..51:11 51:18..51:24 51:28..51:65 /
30 [ConstraintNumber 0] gcc(step_class,cars_in_class)
43:5..43:11 43:32..43:71 44:9..45:38 /
30 [] sliding_sum(0,option_max_per_block[p],option_block_size[p],[step_option_use[1,5],step_option_use[2,5],step_option_use[3,5],step_option_use[4,5],step_option_use[5,5],step_option_use[6,5],step_option_use[7,5],step_option_use[8,5],step_option_use[9,5],step_option_use[10,5]])
51:28..51:65 /
30 [] count(step_class,c,cars_in_class[c])
51:5..51:11 51:18..51:24 51:28..51:65 /
30 [] gcc(step_class,cars_in_class)


-}

import Control.Applicative hiding (many)
import Control.Exception hiding (try)
import Control.Monad
import Data.Either
import Data.Function
import Data.List
import Data.Ord
import qualified Data.Set as S
import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.Printf

type Position = (Integer,Integer)
type Range = (Position,Position)
type ConstraintRange = [Range]
type Replacement = (Double,[Integer],String)

constraintRange :: Parser ConstraintRange
constraintRange = many1 (try range) <* try (tok "/")
range = do
  l1 <- integer
  char ':'
  c1 <- integer
  string ".."
  l2 <- integer
  char ':'
  c2 <- integer
  return ((l1,c1),(l2,c2))

replacement = do
  score <- real
  char ' '
  context <- between (char '[') (char ']') $
               sepBy (string "ConstraintNumber " *> integer) (string ", ")
  char ' '
  rest <- many (noneOf "\n")
  char '\n'
  return (score, context, rest)

tok :: String -> Parser ()
tok s = spc *> string s *> pure ()

pair = do
  cs <- many1 constraintRange <* char '\n'
  r <- replacement <* char '\n'
  return (cs,r)

spc = many (char ' ')

integer :: Parser Integer
integer = do
  spc
  minus <- option "" (string "-")
  digits <- many1 digit
  return $ read (minus ++ digits)

real :: Parser Double
real = do
  spc
  digits1 <- many1 digit
  char '.'
  digits2 <- many1 digit
  return $ read (digits1 ++ "." ++ digits2)

everything :: Parser ([([ConstraintRange], Replacement)], String)
everything = do
  allResults <- many1 $ do
    ranges <- (many1 (try constraintRange)) <?> "constraint ranges"
    tok "\n"
    results <- many (try replacement) <?> "replacements"
    return (map (\r -> (ranges,r)) results)
  string "STATISTICS\n"
  stats <- many anyChar
  return (concat allResults, stats)

main = do
  contents <- getContents
  go contents

go contents = do
--  print contents
  let (parsed,stats) = either (error . show) id $ parse everything  "stdin" contents
  -- evaluate (length contents)
  -- let ls = lines contents
  --     paired = [ (a,b) | (a:b:_) <- tails ls ]
  --     parsed = [ (either (error . show) id $ parse constraintRanges "" a,
  --                 either (error . show) id $ parse replacement "" b) | (a,b) <- paired ]
      keepers = filter (check parsed) parsed
      groupedKeepers = groupBy ((==) `on` fst) keepers
  mapM_ (\g -> putStrLn "GROUP" >> pretty g) groupedKeepers
--  pretty keepers
--  hPutStrLn stderr stats

pretty :: [([ConstraintRange], Replacement)] -> IO ()
pretty = mapM_ $ \(ranges,(scr,ctx,rest)) -> do
           let zippedRanges = zip [0..] ranges
               (contextPart, restPart) =
                   (\(a,b) -> (map snd a, map snd b)) $
                       partition (\(i,r) -> i `elem` ctx) zippedRanges
           putStr $ intercalate " " $ map (\ ((a,b),(c,d)) -> printf "%d:%d..%d:%d" a b c d) (concat contextPart)
           putStr " * "
           putStr $ intercalate " " $ map (\ ((a,b),(c,d)) -> printf "%d:%d..%d:%d" a b c d) (concat restPart)
           putStrLn ""
           printf "%.2f %s\n" scr rest

check :: [([ConstraintRange], Replacement)] -> ([ConstraintRange], Replacement) -> Bool
check parsed (ranges, replacement) =
    not $ any (\ (r,rep) -> (r,rep) /= (ranges,replacement) && r `subrange` ranges && third rep == third replacement) parsed

third (_,_,x) = x

rs1 `subrange` rs2 = S.isSubsetOf (S.fromList (concat rs1)) (S.fromList (concat rs2))

sample = "6:12..6:18 6:25..6:28 6:43..6:63 /\n30 [] element(idx,x,val)\nStatistics {_numberModelEvaluations = 548}\n6:12..6:18 6:36..6:39 6:25..6:28 6:43..6:63 /\n30 [] element(idx,x,val)\nStatistics {_numberModelEvaluations = 344}\n6:12..6:18 6:36..6:39 6:43..6:63 /\nStatistics {_numberModelEvaluations = 546}\n6:43..6:63 /\nStatistics {_numberModelEvaluations = 844}\n"

sample2 = " 6:12..6:18 6:25..6:28 6:43..6:63 / \n30 [] element(idx,x,val)\n 6:12..6:18 6:36..6:39 6:25..6:28 6:43..6:63 / \n30 [] element(idx,x,val)\n 6:12..6:18 6:36..6:39 6:43..6:63 / \n 6:43..6:63 / \n"

sample3 = " 71:1..74:9 / \n 71:1..74:9 / 77:3..77:9 77:16..77:26 78:5..78:40 / \n 71:1..74:9 / 89:3..89:9 89:16..89:29 90:5..90:65 / \n 77:3..77:9 77:16..77:26 78:5..78:40 / \n 77:3..77:9 77:16..77:26 78:5..78:40 / 84:3..84:9 84:16..84:29 85:5..85:28 / \n30 [ConstraintNumber 0] gcc(supplier,use)\n 77:3..77:9 77:16..77:26 78:5..78:40 / 89:3..89:9 89:16..89:29 90:5..90:65 / \n 78:5..78:40 / \n 84:3..84:9 84:16..84:29 85:33..85:53 / \n 84:3..84:9 84:16..84:29 85:5..85:28 / \n30 [] gcc(supplier,use)\n 84:3..84:9 84:16..84:29 85:5..85:28 / 84:3..84:9 84:16..84:29 85:33..85:53 / \n30 [ConstraintNumber 1] gcc(supplier,use)\n 85:33..85:53 / \n 85:5..85:28 / \n 85:5..85:28 / 85:33..85:53 / \n30 [ConstraintNumber 0] atmost(i,supplier,i)\n30 [ConstraintNumber 0] atmost(capacity[i],supplier,i)\n 89:3..89:9 89:16..89:29 90:5..90:65 / \n 89:3..89:9 89:16..89:29 90:5..90:65 / 84:3..84:9 84:16..84:29 85:5..85:28 / \n30 [ConstraintNumber 0] gcc(supplier,use)\n30 [ConstraintNumber 1] lineareq(use,open,n_stores)\n28 [ConstraintNumber 1] lineareq(open,use,n_stores)\n 90:5..90:65 / \n 90:5..90:65 / 85:5..85:28 / \n20 [ConstraintNumber 1] atmost(n_suppliers,[supplier[1],supplier[2],supplier[3],supplier[4],supplier[5],supplier[6],supplier[7],supplier[8],supplier[9],supplier[10]],i)\n18 [ConstraintNumber 1] bin_packing(MaxCost,open,capacity)\n16 [ConstraintNumber 1] bin_packing(MaxTotal,open,capacity)\n15 [ConstraintNumber 1] cumulative_assert(open,capacity,use,MaxTotal)\n18 [ConstraintNumber 1] lex_less_int_checking(open,capacity)\nSTATISTICS\nStatistics {_numberModelEvaluations = 128270, _numberFlatZincCalls = 2094, _labelledTime = fromList [(\"evaluation\",62684),(\"finding intersection\",387040),(\"getGoodConstraints\",205488),(\"solve\",136631),(\"whole program\",390871)]}"

sample4 =
  " 71:1..74:9 / \n 71:1..74:9 / 77:3..77:9 77:16..77:26 78:5..78:40 / \n 71:1..74:9 / 89:3..89:9 89:16..89:29 90:5..90:65 / \n 77:3..77:9 77:16..77:26 78:5..78:40 / \n 77:3..77:9 77:16..77:26 78:5..78:40 / 84:3..84:9 84:16..84:29 85:5..85:28 / \n30 [ConstraintNumber 0] gcc(supplier,use)\n 77:3..77:9 77:16..77:26 78:5..78:40 / 89:3..89:9 89:16..89:29 90:5..90:65 / \n 78:5..78:40 / \n 84:3..84:9 84:16..84:29 85:33..85:53 / \n 84:3..84:9 84:16..84:29 85:5..85:28 / \n30 [] gcc(supplier,use)\n 84:3..84:9 84:16..84:29 85:5..85:28 / 84:3..84:9 84:16..84:29 85:33..85:53 / \n30 [ConstraintNumber 1] gcc(supplier,use)\n 85:33..85:53 / \n 85:5..85:28 / \n 85:5..85:28 / 85:33..85:53 / \n25 [ConstraintNumber 0] atmost(i,supplier,i)\n30 [ConstraintNumber 0] atmost(capacity[i],supplier,i)\n 89:3..89:9 89:16..89:29 90:5..90:65 / \n 89:3..89:9 89:16..89:29 90:5..90:65 / 84:3..84:9 84:16..84:29 85:5..85:28 / \n30 [ConstraintNumber 0] gcc(supplier,use)\n29 [ConstraintNumber 1] lineareq(use,open,n_stores)\n29 [ConstraintNumber 1] lineareq(open,use,n_stores)\n 90:5..90:65 / \n18 [] lex_less_int_checking(open,capacity)\n22 [] sliding_sum(capacity[n_suppliers],building_cost,n_suppliers,supplier)\n 90:5..90:65 / 85:5..85:28 / \n18 [ConstraintNumber 1] atmost(n_suppliers,supplier,n_suppliers)\n18 [ConstraintNumber 1] atmost(n_suppliers,supplier,capacity[i])\n16 [ConstraintNumber 1] bin_packing(building_cost,open,capacity)\n15 [ConstraintNumber 1] bin_packing(n_stores,use,capacity)\n19 [ConstraintNumber 1] bin_packing(MaxCost,use,capacity)\n15 [ConstraintNumber 1] bin_packing(MaxTotal,use,capacity)\n16 [ConstraintNumber 1] bin_packing(building_cost,use,capacity)\n17 [ConstraintNumber 1] cumulative_assert(open,capacity,use,MaxCost)\n19 [ConstraintNumber 1] cumulative_assert(open,capacity,use,MaxTotal)\n18 [ConstraintNumber 1] cumulative_assert(open,use,capacity,MaxTotal)\n15 [ConstraintNumber 1] cumulative_assert(open,use,capacity,building_cost)\n16 [ConstraintNumber 1] lex_lesseq_int_checking(open,capacity)\n15 [ConstraintNumber 1] sliding_sum(0,n_stores,capacity[n_suppliers],open)\n16 [ConstraintNumber 1] sliding_sum(capacity[i],building_cost,n_suppliers,supplier)\nSTATISTICS\nStatistics {_numberModelEvaluations = 124780, _numberFlatZincCalls = 2017, _numberSuccessfulImpliesChecks = 0, _labelledTime = fromList [(\"evaluation\",29083),(\"finding intersection\",197294),(\"getGoodConstraints\",141802),(\"solve\",108573),(\"whole program\",198934)]}"
