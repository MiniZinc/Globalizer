module ParseAssignI where

import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad
import Language.MiniZinc
import Text.Parsec
import Text.Parsec.String

import Debug.Trace

p :: Parser Item
p = do
  i <- identifier
  spaces
  tok "="
  spaces
  v <- rhs
  return $ AssignI i v

identifier :: Parser String
identifier = do
  x <- letter
  xs <- many (alphaNum <|> char '_')
  return (x:xs)

rhs :: Parser Expression
rhs = arrayNd 1 <|> arrayNd 2 <|> arrayNd 3 <|> (IntLit <$> integer)

-- array1d = do
--   tok "array1d"
--   xs <- between (tok "(") (tok ")") $ do
--           integer
--           tok ".."
--           integer
--           tok ","
--           spaces
--           between (tok "[") (tok "]") $ do
--             sepBy (spaces >> integer) (spaces >> tok ",")
--   return $ arrayLit (xs :: [Int])

-- [["isGuest = array3d(1..3, 1..3, 1..3, [0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1]);","isHost = array1d(1..3, [1, 0, 1]);","location = array2d(1..3, 1..3, [1, 1, 1, 1, 1, 3, 3, 3, 3]);"],["isGuest = array3d(1..3, 1..3, 1..3, [1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0]);","isHost = array1d(1..3, [1, 0, 1]);","location = array2d(1..3, 1..3, [1, 1, 1, 1, 3, 3, 3, 3, 3]);"],["isGuest = array3d(1..3, 1..3, 1..3, [1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1]);","isHost = array1d(1..3, [1, 0, 1]);","location = array2d(1..3, 1..3, [1, 1, 1, 1, 1, 1, 3, 3, 3]);"],["isGuest = array3d(1..3, 1..3, 1..3, [1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0]);","isHost = array1d(1..3, [1, 0, 1]);","location = array2d(1..3, 1..3, [1, 1, 1, 1, 3, 3, 3, 3, 3]);"],["isGuest = array3d(1..3, 1..3, 1..3, [1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1]);","isHost = array1d(1..3, [1, 0, 1]);","location = array2d(1..3, 1..3, [1, 1, 1, 3, 3, 1, 3, 3, 3]);"]]

-- array2d = do
--   tok "array2d"
--   between (tok "(") (tok ")") $ do
--       dims <- replicateM 3 (dim <* comma)
--       vals <- between (tok "[") (tok "]") $ do
--         sepBy (spaces >> integer) (spaces >> tok ",")
--       return $ ArrayLit (map IntLit vals) dims

arrayNd :: Int -> Parser Expression
arrayNd n = do
  try (tok $ "array" ++ show n ++ "d")
  between (tok "(") (tok ")") $ do
      dims <- replicateM n (dim <* comma)
      vals <- between (tok "[") (tok "]") $ sepBy integer comma
      return $ ArrayLit (map IntLit vals) dims

dim :: Integral a => Parser (a,a)
dim = do
  l <- integer
  tok ".."
  u <- integer
  return (fromInteger l, fromInteger u)

tok :: String -> Parser ()
tok s = spaces *> string s *> pure ()

comma :: Parser ()
comma = tok ","

integer :: Parser Integer
integer = do
  spaces
  minus <- option "" (string "-")
  digits <- many1 digit
  return $ read (minus ++ digits)

parseAssignI :: String -> Either ParseError Item
parseAssignI s = parse p "" s

-- q_1 = array1d(1..10, [10, 3, 4, 7, 8, 6, 5, 9, 2, 1]);
