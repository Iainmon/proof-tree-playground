module Hoohui.Parser where


import Logic.Unification.Basic

import Text.Parsec hiding (runP)
import qualified Text.Parsec as P
-- import Text.Parsec.String (Parser)
import Control.Monad (void)

data ParseState 
  = ParseState { mixFixSpecs :: [MixFixSpec] }
  deriving Show

emptyParseState = ParseState []



type Parser = Parsec String ParseState


enclosedIn :: Parser a -> Parser b -> Parser c -> Parser c
enclosedIn o c p = do
  _ <- o
  x <- lexeme p
  _ <- c
  return x

lexeme :: Parser a -> Parser a
lexeme = between spaces spaces

symbol :: Char -> Parser ()
symbol c = lexeme (do { char c; return () }) <?> "symbol " ++ show [c]

symbols :: String -> Parser ()
symbols s = lexeme (do { string s; return () }) <?> "symbol " ++ show s

braces :: Parser a -> Parser a
braces = enclosedIn (symbol '{') (symbol '}')

brackets :: Parser a -> Parser a
brackets = enclosedIn (symbol '[') (symbol ']')

parens :: Parser a -> Parser a
parens = enclosedIn (symbol '(') (symbol ')')

nameParser :: Parser Name
nameParser = do
  spaces
  n <- many1 (alphaNum <|> char '_') <?> "name"
  spaces
  return n

termVarParser :: Parser BTerm
termVarParser = (Var <$> braces nameParser) <?> "variable"
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
  n <- lexeme (read <$> many1 digit) <?> "number"
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

mixFixTermParser :: Parser BTerm
mixFixTermParser = do
  mfSpecs <- getState >>= return . mixFixSpecs
  choice $ map (\mf -> (lexeme $ parseMixFixSpec mf)) mfSpecs

termParser :: Parser BTerm
termParser = spaces *> ((try termVarParser <?> "var term")
                    -- <|> (try mixFixTermParser <?> "mixfix")
                    <|> (try termTermParser <?> "term")
                    <|> (try termNumLitParser <?> "number")
                    <|> (try termLitParser <?> "literal")
                    ) <* spaces

clauseParser :: Parser BTerm
clauseParser = do
  try mixFixTermParser <|> termParser

ruleParser :: Parser BRule
ruleParser = do
  ruleName <- brackets nameParser
  conclusion <- lexeme clauseParser
  lexeme $ string "-:"
  premises <- lexeme clauseParser `sepBy` symbol ','
  lookAhead (symbol ';')
  return (Rule ruleName conclusion premises)

stmtParser :: Parser [BRule]
stmtParser = many $ do
  try $ many $ lexeme mixFixSpec >> (many1 (symbol ';'))
  r <- lexeme ruleParser
  -- r <- try (lexeme ruleParser) <|> (do {
  --   try (lexeme mixFixSpec `endBy` (many1 (symbol ';')));
  --   lexeme ruleParser
  -- })
  (many1 (symbol ';'))
  return r

rulesParser :: Parser [BRule]
rulesParser = do
  rs <- stmtParser
  -- rs <- lexeme ruleParser `endBy` (many1 (symbol ';'))
  eof
  return rs


state :: Parser String
state = getParserState >>= return  . stateInput

runP :: Parser a -> String -> a
runP p s = case P.runP p emptyParseState "" s of
  Left err -> error $ show err -- ++ "\n\n" ++ (show $ runP (try (try (try p >> return ()) <|> getState)) s)
  Right a -> a

runP' :: Parser a -> String -> a
runP' p s = case P.runP p' emptyParseState "" s of
              Left err -> error $ show err -- ++ "\n\n" ++ (show $ runP (try (try (try p >> return ()) <|> getState)) s)
              Right a -> a
  where p' = do { a <- p; eof; return a }


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
\ [rule3] knows({x},{y}) -: knows({x},{z}) , knows({z},{y}) ;   \n\
\ `{G} |- {E}` :-: type({G},{E}) ; \n\
\ [rule3] g |- e -:  ;      "


-- Mixfix specifecation:
-- "{G} |- {E} : {T}" :-: typingRelation({G},{E},{T}) ;

type MFSymbol = String
type MFTextSymbol = String

data MixFixPatPart 
  = MFPSym MFSymbol
  | MFPTxt MFTextSymbol
  | MFPVar Name
  deriving Show

type MixFixPattern = [MixFixPatPart]
data MixFixSpec = MixFixSpec MixFixPattern BTerm deriving Show

mfTextSymbol :: Parser MFTextSymbol
mfTextSymbol = do
  notFollowedBy (lexeme $ char '{')
  lexeme $ many1 (letter <|> char '_' <|> char '-')


allowedSymbols = ":,|-=+*/^&!<>~@#$%?"
isSymbol s = all (`elem` allowedSymbols) s

mfSymbol :: Parser MFSymbol
mfSymbol = lexeme $ many1 (oneOf allowedSymbols)

mfVar :: Parser Name
mfVar = lexeme (braces nameParser)

mixFixPatternPart :: Parser MixFixPatPart
mixFixPatternPart = do
  -- notFollowedBy (lexeme $ string ":-:")
  (try (MFPVar <$> mfVar) <?> "mix fix variable")
    <|> (try (MFPSym <$> mfSymbol) <?> "mix fix symbol")
    <|> (try (MFPTxt <$> mfTextSymbol) <?> "mix fix text symbol")

mixFixPattern :: Parser MixFixPattern
mixFixPattern = many1 mixFixPatternPart

mixFixSpec :: Parser MixFixSpec
mixFixSpec = do
  symbol '`'
  p <- lexeme mixFixPattern
  symbol '`'
  lexeme $ string ":-:"
  t <- lexeme termParser
  lookAhead (symbol ';')
  let spec = MixFixSpec p t
  modifyState (\s -> s { mixFixSpecs = spec : mixFixSpecs s })
  return $ spec


parseMixFixPattern :: MixFixPattern -> Parser [(String,BTerm)]
parseMixFixPattern [] = do
  -- lookAhead $ symbols "-:" <|> symbol ';'
  -- notFollowedBy $ (void (lexeme nameParser)) <|> void (lexeme letter)
  return []
parseMixFixPattern (p:ps) = do
  case p of
    MFPSym s -> do
      symbols s
      parseMixFixPattern ps
    MFPTxt s -> do
      symbols s
      parseMixFixPattern ps
    MFPVar v -> do
      t <- lexeme termParser
      ts <- parseMixFixPattern ps
      return $ (v,t):ts

parseMixFixSpec :: MixFixSpec -> Parser BTerm
parseMixFixSpec (MixFixSpec p t) = do
  ps <- parseMixFixPattern p
  return $ foldl (\t (s,t') -> subst s t' t) t ps



operator :: String -> Parser String
operator s = lexeme $ string s



symbolP :: Parser String
symbolP = lexeme $ many1 (oneOf allowedSymbols)


symbolsPP :: Parser (String,Parser String)
symbolsPP = do
  -- notFollowedBy (lexeme $ string "-:")
  s <- lexeme $ symbolP
  return $ (s,symbols s >> return s)

mixfixVarPP :: Parser (String,Parser BTerm)
mixfixVarPP = do
  -- notFollowedBy (lexeme $ string "-:")
  s <- lexeme nameParser
  return $ (s,lexeme termParser)

alternate :: Parser a -> Parser a -> Parser [a]
alternate p1 p2 = do
  a <- try (Left <$> p1) <|> (Right <$> p2)
  case a of
    Left a -> do
      as <- alternate p1 p2
      return (a:as)
    Right a -> do
      as <- alternate p2 p1
      return (a:as)

compilePatternPP :: Parser (Parser [(String,Either String BTerm)])
compilePatternPP = do
  ps <- many1 $ try (mapP mixfixVarPP Right) <|> (mapP symbolsPP Left)
  return $ sequence $ map (\(s,p) -> p >>= \t -> return (s,t)) ps
    where mapP p f = p >>= \(s,p') -> return (s,p' >>= return . f)
          varPP = mapP mixfixVarPP Right
          symPP = mapP symbolsPP Left
          -- scan = do
          --   try scan1 <|> (do {symp <- symPP; ps <- scan1 ; return (symp:ps)})
          -- scan1 = do
          --   varp <- varPP
          --   ps <- scan
          --   return (symp:varp:ps)
mixfixPatternPP :: Parser (Parser [(String,BTerm)])
mixfixPatternPP = do
  p <- lexeme $ compilePatternPP
  eof
  return $ do
    mps <- p
    eof
    return [(s,t) | (s,Right t) <- mps]

-- mixfixParser :: Parser (Parser BTerm)
-- mixfixParser = do
--   -- lexeme $ char '['
--   p <- lexeme mixfixPatternPP
--   -- lexeme $ char ']'
--   lexeme $ string ":-:"
--   t <- lexeme termParser
--   lookAhead (symbol ';')
--   return $ do
--     mps <- lexeme p <?> "pattern"
--     return $ foldl (\t (s,t') -> subst s t' t) t mps


-- p1 = do
--   x <- runP mixfixPatternPP "G |- E : T"
--   eof
--   return x

-- p2 = do
--   x <- runP mixfixParser "G |- E : T :-: type({G},{E},{T}) ;"
--   return x

-- p3 = runP mixFixSpec "G |- E : T :-: type({G},{E},{T}) ;"

p1 = runP mixFixSpec "`{G} |- {E} => {V} has-type {T}` :-: type({G},{E},{T}) ;"