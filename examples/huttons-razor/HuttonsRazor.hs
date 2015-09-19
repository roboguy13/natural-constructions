{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}

-- Based on a slight extension of Hutton's razor

---- Examples ----
--  * Example 1:
--      ghci> parseEvalAndShow  "1 + 2 + 3 + 4"
--      "10"
--
--  * Example 2:
--      ghci> parseEvalAndShow  "2 + 4 = 1 + 2 + 1 + 1 + 1"
--      "True"
--
--  * Example 3:
--      ghci> parseEvalAndShow  "2 + 4 = 1 + 6 + 7"
--      "False"

import           Data.Natural

import           Text.Parsec
import           Text.Parsec.String

import           Control.Monad
import           Control.Monad.Identity

-- Unfortunately, this doesn't work with an open data family, so a GADT
-- must be used here.
-- TODO: See if there's a way to use an open data family with the rest of
-- the code.
data Sing a where
  IntS  :: Sing Int
  BoolS :: Sing Bool

-- data family Sing a
-- data instance Sing Int = IntS
-- data instance Sing Bool = BoolS

data Expr a where
  Literal :: Int                  -> Expr Int
  Add     :: Expr Int -> Expr Int -> Expr Int
  Equal   :: Expr Int -> Expr Int -> Expr Bool

eval :: Expr a -> a
eval (Literal n) = n
eval (Add   x y) = eval x + eval y
eval (Equal x y) = eval x == eval y

parseAndEval :: String -> (Sing :** Identity)
parseAndEval str
  = case parseExpr str of
      IntS  :** x -> IntS :** Identity (eval x)
      BoolS :** x -> BoolS :** Identity (eval x)

parseEvalAndShow :: String -> String
parseEvalAndShow str
  = case parseAndEval str of
      IntS  :** Identity n -> show n
      BoolS :** Identity b -> show b

parseExpr :: String -> Sing :** Expr
parseExpr str
  = case parse exprParser "" str of
      Left err -> error $ "Parse error: " ++ show err
      Right r  -> r

exprParser :: Parser (Sing :** Expr)
exprParser
  = choice $ map try
      [ fmap (BoolS :**) parseEqual
      , fmap (IntS  :**) parseAdd
      , fmap (IntS  :**) parseLiteral
      ]

parseLiteral :: Parser (Expr Int)
parseLiteral = do
  literal <- read <$> many1 digit
  return $ Literal literal

parseExprInt :: Parser (Expr Int)
parseExprInt = try parseAdd <|> try parseLiteral

parseBinaryOp :: Parser a -> Parser a -> (a -> a -> b) -> Char -> Parser b
parseBinaryOp p q op opName = do
  a <- p
  many space
  char opName
  many space
  b <- q
  return $ op a b

parseAdd :: Parser (Expr Int)
parseAdd = parseBinaryOp parseLiteral parseExprInt Add '+'

parseEqual :: Parser (Expr Bool)
parseEqual = parseBinaryOp parseExprInt parseExprInt Equal '='

