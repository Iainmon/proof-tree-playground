module Parser where


import Text.Parsec (ParseError, try, parse, alphaNum, many1, (<|>), eof, runParser)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, char, digit, satisfy)


import Syntax


parseExpr :: Parser Expr
parseExpr = try parseLit <|> try parsePair

parseLit :: Parser Expr
parseLit = do
  n <- many1 digit
  return $ Lit (read n)


parsePair :: Parser Expr
parsePair = do
  char '('
  e1 <- parseExpr
  char ','
  e2 <- parseExpr
  char ')'
  return $ Pair e1 e2


parse' :: String -> Either ParseError Expr
parse' s = runParser parseExpr () "(string)" s

parse :: String -> Expr
parse s = case parse' s of
  Left err -> error $ show err
  Right e -> e