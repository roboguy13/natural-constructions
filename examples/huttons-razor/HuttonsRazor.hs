{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeOperators  #-}
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
--
--  * Example 4:
--      ghci> endApply showExpr . natSnd $ parseExpr "2 + 4 = 6"
--      "Literal 2 + Literal 4 = Literal 6"

import           Data.Natural

import           Text.Parsec
import           Text.Parsec.String

import           Control.Monad.Identity

data Expr a where
  Literal :: Int                  -> Expr Int
  Add     :: Expr Int -> Expr Int -> Expr Int
  Equal   :: Expr Int -> Expr Int -> Expr Bool

--- Unfortunately, this doesn't work with an open data family, so a GADT
--- must be used here.
--- TODO: See if there's a way to use an open data family with the rest of
--- the code.
data ExprSing a where
  IntS  :: ExprSing Int
  BoolS :: ExprSing Bool

eval :: Expr a -> a
eval (Literal n) = n
eval (Add   x y) = eval x + eval y
eval (Equal x y) = eval x == eval y

showExpr :: Expr a -> String
showExpr (Literal n) = "Literal " ++ show n
showExpr (Add x y)   = showExpr x ++ " + " ++ showExpr y
showExpr (Equal x y) = showExpr x ++ " = " ++ showExpr y

withParsedExpr :: (forall a. Expr a -> b) -> String -> b
withParsedExpr f = endApply f . natSnd . parseExpr

parseEvalAndShow :: String -> String
parseEvalAndShow str
  = case parseAndEval str of
      IntS  :** Identity n -> show n
      BoolS :** Identity b -> show b

parseAndEval :: String -> ExprSing :** Identity
parseAndEval str
  = natSecond' (Identity . eval) (parseExpr str)

parseExpr :: String -> ExprSing :** Expr
parseExpr str
  = case parse exprParser "" str of
      Left err -> error $ "Parse error: " ++ show err
      Right r  -> r

exprParser :: Parser (ExprSing :** Expr)
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

