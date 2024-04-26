module Hoohui.Parser where


import Logic.Unification.Basic

import Text.Parsec hiding (runP)
import qualified Text.Parsec as P
import Text.Parsec.Prim (setInput)
-- import Text.Parsec.String (Parser)
import Control.Monad (void,msum)

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
  choice $ map (\mf -> try (lexeme $ parseMixFixSpec mf)) mfSpecs

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
runP p s = case P.runParser p emptyParseState "" s of
  Left err -> error $ show err -- ++ "\n\n" ++ (show $ runP (try (try (try p >> return ()) <|> getState)) s)
  Right a -> a

runP' :: Parser a -> String -> a
runP' p s = case P.runP p' emptyParseState "" s of
              Left err -> error $ show err -- ++ "\n\n" ++ (show $ runP (try (try (try p >> return ()) <|> getState)) s)
              Right a -> a
  where p' = do { a <- p; eof; return a }


parseAll :: String -> String -> (RuleSystem Name, BTerm, ParseState)
parseAll src trm = runP p ""
  where p = do 
          setInput src
          rs <- rulesParser
          setInput trm
          t <- clauseParser
          s <- getState
          return (rs,t,s)

parseTerm :: String -> BTerm
parseTerm = runP termParser

parseTerm' :: String -> String -> BTerm
parseTerm' src t = let s = runP (rulesParser >> getState) src 
                    in runP (do {putState s ; t' <- clauseParser; eof; return t'}) t
                    -- in error $ show s -- runP (do {putState s ; t' <- clauseParser; eof; return t'}) t

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
  notFollowedBy (lexeme $ string ":-:")
  (try (MFPVar <$> mfVar) <?> "mix fix variable")
    <|> (try (MFPSym <$> mfSymbol) <?> "mix fix symbol")
    <|> (try (MFPTxt <$> mfTextSymbol) <?> "mix fix text symbol")

mixFixPattern :: Parser MixFixPattern
mixFixPattern = many1 $ lexeme mixFixPatternPart

mixFixSpec :: Parser MixFixSpec
mixFixSpec = do
  symbol '`'
  p <- lexeme mixFixPattern
  symbol '`'
  lexeme $ string ":-:"
  t <- lexeme termTermParser
  lookAhead (symbol ';')
  let spec = MixFixSpec p t
  modifyState (\s -> s { mixFixSpecs = mixFixSpecs s ++ [spec]})
  return $ spec


parseMixFixPattern :: MixFixPattern -> Parser [(String,BTerm)]
parseMixFixPattern [] = do
  -- lookAhead $ symbols "-:" <|> symbol ';'
  notFollowedBy $ (void (lexeme termParser)) <|> void (lexeme letter)
  return []
parseMixFixPattern (p:ps) = do
  case p of
    MFPSym s -> do
      lexeme $ symbols s
      parseMixFixPattern ps
    MFPTxt s -> do
      lexeme $ symbols s
      parseMixFixPattern ps
    MFPVar v -> do
      t <- lexeme termParser
      ts <- parseMixFixPattern ps
      -- notFollowedBy $ (void (lexeme termParser)) <|> void (lexeme letter)
      return $ (v,t):ts

parseMixFixSpec :: MixFixSpec -> Parser BTerm
parseMixFixSpec (MixFixSpec p t) = do
  ps <- lexeme $ parseMixFixPattern p
  return $ foldl (\t (s,t') -> subst s t' t) t ps



operator :: String -> Parser String
operator s = lexeme $ string s


checkSpec :: BTerm -> MixFixSpec -> Maybe (MixFixPattern, Subst Name)
checkSpec t spec@(MixFixSpec p t') = do
  s <- safeUnify t' t
  return (p,s)

toInfix :: BTerm -> [MixFixSpec] -> Maybe (MixFixPattern, Subst Name)
toInfix t specs = msum $ map (checkSpec t) specs



p1 = runP mixFixSpec "`{G} |- {E} => {V} has-type {T}` :-: type({G},{E},{T}) ;"