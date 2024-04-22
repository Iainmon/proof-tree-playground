module Hoohui.Parser where


import Logic.Unification.Basic

import Text.Parsec hiding (runP)
import Text.Parsec.String (Parser)

enclosedIn :: Parser a -> Parser b -> Parser c -> Parser c
enclosedIn o c p = do
  _ <- o
  x <- lexeme p
  _ <- c
  return x

lexeme :: Parser a -> Parser a
lexeme = between spaces spaces

symbol :: Char -> Parser ()
symbol c = lexeme (do { char c; return () })

braces :: Parser a -> Parser a
braces = enclosedIn (symbol '{') (symbol '}')

brackets :: Parser a -> Parser a
brackets = enclosedIn (symbol '[') (symbol ']')

parens :: Parser a -> Parser a
parens = enclosedIn (symbol '(') (symbol ')')

nameParser :: Parser Name
nameParser = do
  spaces
  n <- many1 (alphaNum <|> char '_')
  spaces
  return n

termVarParser :: Parser BTerm
termVarParser = Var <$> braces nameParser
-- termVarParser = do
--   char '{'
--   spaces
--   n <- nameParser
--   spaces
--   char '}'
--   spaces
--   return (Var n)

termNumLitParser :: Parser BTerm
termNumLitParser = do
  n <- lexeme (read <$> many1 digit)
  lexeme $ notFollowedBy (char '(')
  return (consNum n)
  where consNum 0 = Term "zero" []
        consNum n = Term "succ" [consNum (n-1)]


termLitParser :: Parser BTerm
termLitParser = do
  f <- lexeme nameParser
  lexeme $ notFollowedBy (char '(')
  return (Term f [])

termTermParser :: Parser BTerm
termTermParser = do
  n <- nameParser
  ts <- parens $ termParser `sepBy` (spaces *> symbol ',' <* spaces)
  return (Term n ts)

termParser :: Parser BTerm
termParser = spaces *> ((try termVarParser <?> "var term")
                    <|> (try termTermParser <?> "term")
                    <|> (try termNumLitParser <?> "number")
                    <|> (try termLitParser <?> "literal")) <* spaces


ruleParser :: Parser BRule
ruleParser = do
  ruleName <- brackets nameParser
  conclusion <- lexeme termParser
  lexeme $ string "-:"
  premises <- lexeme termParser `sepBy` symbol ','
  lookAhead (symbol ';')
  return (Rule ruleName conclusion premises)

rulesParser :: Parser [BRule]
rulesParser = do
  rs <- lexeme ruleParser `endBy` (many1 (symbol ';'))
  eof
  return rs


state :: Parser String
state = getParserState >>= return  . stateInput

runP :: Parser a -> String -> a
runP p s = case parse p "" s of
  Left err -> error $ show err -- ++ "\n\n" ++ (show $ runP (try (try (try p >> return ()) <|> getState)) s)
  Right a -> a


parseTerm :: String -> BTerm
parseTerm = runP termParser

parseRule :: String -> BRule
parseRule = runP ruleParser

parseRules :: String -> [BRule]
parseRules = runP rulesParser

parseRuleSystem :: String -> RuleSystem Name
parseRuleSystem = parseRules

rs = "[rule1] knows(Alice,Bob) -: ; \n\
\ [rule2] knows(Bob,Charlie) -: ;  \n\
\ [rule3] knows({x},{y}) -: knows({x},{z}) , knows({z},{y}) ;   \n  "