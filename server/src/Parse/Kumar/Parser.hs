{-# OPTIONS_GHC -dno-suppress-type-signatures -ddump-splices #-}

{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}


module Parse.Kumar.Parser where
  -- (..)

-- myLexer,
-- pGrammar,
-- pListDef,
-- pDef,
-- pListCat,
-- pLabel,
-- pCat,
-- pListItem,
-- pItem,
-- pString



import Language.Haskell.TH (Q,Dec,Exp)
import Language.LBNF
import Data.List (lines)


bnfc [lbnf|


position token UIdent (upper (letter | digit | '_')*);
position token LIdent (lower (letter | digit | '_')*);
position token Number (('-' (digit+)) | (digit+));
position token BinOp  (('+' | '-' | '*' | '/' | '=' | '<' | '>' | '.') | (('+' | '-' | '*' | '/' | '=' | '<' | '>' | '.' | ':') ('+' | '-' | '*' | '/' | '=' | '<' | '>' | '.' | ':')+));
position token BinOp7P ('*' | '/');
position token BinOp6P ('+' | '-');
position token BinOp5P (':' | ('+' '+'));
position token BinOp4P ((('<' | '>' | '=') ('=')) | '<' | '>' );
position token Colon   (':' | [":"]);


EVar.            Expr10 ::= LIdent ;
EInt.            Expr10 ::= Number ;
ECon.            Expr10 ::= UIdent ;
EStr.            Expr10 ::= String ;
EList.           Expr10 ::= "[" [Expr] "]" ;
EBinFn.          Expr10 ::= "(" BinOp ")" ;
EFun.            Expr10 ::= "fun" LIdent "->" Expr ;
ELet.            Expr10 ::= "let" [Decl] "in" Expr ;
ECase.           Expr10 ::= "case" Expr "of" "{" [CaseAlt] "}" ;
EIf.             Expr10 ::= "if" Expr "then" Expr "else" Expr ;
EApp.            Expr9 ::= Expr9 Expr10 ;
EBinOp8.         Expr8 ::= Expr8 BinOp Expr9 ;
EBinOp7.         Expr7 ::= Expr7 BinOp7P Expr8 ;
EBinOp6.         Expr6 ::= Expr6 BinOp6P Expr7 ;
EBinOp5.         Expr5 ::= Expr6 BinOp5P Expr5 ; -- Right assoc
EBinOp4.         Expr4 ::= Expr4 BinOp4P Expr5 ;

coercions Expr 10 ;
separator Expr "," ;

(:[]).           [LIdent] ::= LIdent;
(:).             [LIdent] ::= LIdent [LIdent];

DSimp.           Decl ::= LIdent "=" Expr ;
DSimpFun.        Decl ::= LIdent [LIdent] "=" Expr ;
DRec.            Decl ::= "rec" LIdent [LIdent] "=" Expr ;
DType.           Decl ::= "data" UIdent "=" [ConDef] ;
(:[]).           [Decl] ::= Decl;
(:).             [Decl] ::= Decl "and" [Decl] ;

ConDef.          ConDef ::= UIdent [TypeName] ;
(:[]).           [ConDef] ::= ConDef;
(:).             [ConDef] ::= ConDef "|" [ConDef] ;

TNLit.           TypeName ::= UIdent ;
TNList.          TypeName ::= "[" TypeName "]" ;
[].              [TypeName] ::= ;
(:).             [TypeName] ::= TypeName [TypeName] ;

CaseAlt.         CaseAlt ::= Pattern "->" Expr ;
(:[]).           [CaseAlt] ::= CaseAlt;
(:).             [CaseAlt] ::= CaseAlt ";" [CaseAlt] ;

PVar.           Pattern1 ::= LIdent ;
PInt.           Pattern1 ::= Number ;
PAny.           Pattern1 ::= "_" ;
PListNil.       Pattern1 ::= "[" "]" ;
PCons.          Pattern1 ::= UIdent [Pattern];
PListCons.      Pattern  ::= Pattern1 BinOp5P Pattern ;
coercions Pattern 1 ;
separator Pattern " " ;

Col.           ColonNT ::= Colon ;

entrypoints Expr, Decl, CaseAlt, Pattern ;


  |]

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

clean ('\\':'n':s) = ' ' : clean s
clean (c:s) = c : clean s
clean [] = []

tokenize :: String -> [Token]
tokenize = myLexer . clean

parseExpr :: String -> Expr
parseExpr s = case pExpr (tokenize s) of
  Ok e -> e
  Bad s -> error s

parseExprSafe :: String -> Either String Expr
parseExprSafe s = case pExpr (tokenize s) of
  Ok e -> Right e
  Bad err -> Left err



{-
caseAlt ::
  template-haskell-2.19.0.0:Language.Haskell.TH.Quote.QuasiQuoter
decl ::
  template-haskell-2.19.0.0:Language.Haskell.TH.Quote.QuasiQuoter
expr ::
  template-haskell-2.19.0.0:Language.Haskell.TH.Quote.QuasiQuoter
myLexer :: String -> [Token]
pCaseAlt :: [Token] -> Language.LBNF.Runtime.ParseMonad CaseAlt
pDecl :: [Token] -> Language.LBNF.Runtime.ParseMonad Decl
pExpr :: [Token] -> Language.LBNF.Runtime.ParseMonad Expr
pPattern :: [Token] -> Language.LBNF.Runtime.ParseMonad Pattern
pattern ::
  template-haskell-2.19.0.0:Language.Haskell.TH.Quote.QuasiQuoter
qCaseAlt ::
  [Token]
  -> Language.LBNF.Runtime.ParseMonad
       Language.LBNF.Compiletime.BNFC_QQType
qDecl ::
  [Token]
  -> Language.LBNF.Runtime.ParseMonad
       Language.LBNF.Compiletime.BNFC_QQType
qExpr ::
  [Token]
  -> Language.LBNF.Runtime.ParseMonad
       Language.LBNF.Compiletime.BNFC_QQType
qPattern ::
  [Token]
  -> Language.LBNF.Runtime.ParseMonad
       Language.LBNF.Compiletime.BNFC_QQType
tokens :: String -> [Token]
type AlexAcc :: * -> * -> *
data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)
type AlexAccPred :: * -> *
type AlexAccPred user =
  user
  -> Language.LBNF.Compiletime.AlexInput
  -> Int
  -> Language.LBNF.Compiletime.AlexInput
  -> Bool
type AlexLastAcc :: * -> *
data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !Language.LBNF.Compiletime.AlexInput !Int
  | AlexLastSkip !Language.LBNF.Compiletime.AlexInput !Int
type AlexReturn :: * -> *
data AlexReturn a
  = AlexEOF
  | AlexError !Language.LBNF.Compiletime.AlexInput
  | AlexSkip !Language.LBNF.Compiletime.AlexInput !Int
  | AlexToken !Language.LBNF.Compiletime.AlexInput !Int a
type BTree :: *
data BTree = N | B String Tok BTree BTree
type BinOp :: *
newtype BinOp = BinOp ((Int, Int), String)
type BinOp4P :: *
newtype BinOp4P = BinOp4P ((Int, Int), String)
type BinOp5P :: *
newtype BinOp5P = BinOp5P ((Int, Int), String)
type BinOp6P :: *
newtype BinOp6P = BinOp6P ((Int, Int), String)
type BinOp7P :: *
newtype BinOp7P = BinOp7P ((Int, Int), String)
type CaseAlt :: *
data CaseAlt = CaseAlt Pattern Expr
type ConDef :: *
data ConDef = ConDef UIdent [TypeName]
type Decl :: *
data Decl
  = DSimp LIdent Expr
  | DRec LIdent LIdent Expr
  | DType UIdent [ConDef]
type Expr :: *
data Expr
  = EVar LIdent
  | EInt Number
  | ECon UIdent
  | EStr String
  | EList [Expr]
  | EBinFn BinOp
  | EFun LIdent Expr
  | ELet [Decl] Expr
  | ECase Expr [CaseAlt]
  | EIf Expr Expr Expr
  | EApp Expr Expr
  | EBinOp8 Expr BinOp Expr
  | EBinOp7 Expr BinOp7P Expr
  | EBinOp6 Expr BinOp6P Expr
  | EBinOp5 Expr BinOp5P Expr
  | EBinOp4 Expr BinOp4P Expr
type HappyAbsSyn :: *
data HappyAbsSyn
  = HappyTerminal Token
  | HappyErrorToken Int
  | HappyAbsSyn11 String
  | HappyAbsSyn12 Language.LBNF.Compiletime.BNFC_QQType
  | HappyAbsSyn13 UIdent
  | HappyAbsSyn15 LIdent
  | HappyAbsSyn17 Number
  | HappyAbsSyn19 BinOp
  | HappyAbsSyn21 BinOp7P
  | HappyAbsSyn23 BinOp6P
  | HappyAbsSyn25 BinOp5P
  | HappyAbsSyn27 BinOp4P
  | HappyAbsSyn29 Expr
  | HappyAbsSyn51 [Expr]
  | HappyAbsSyn53 Decl
  | HappyAbsSyn55 [Decl]
  | HappyAbsSyn57 ConDef
  | HappyAbsSyn59 [ConDef]
  | HappyAbsSyn61 TypeName
  | HappyAbsSyn63 [TypeName]
  | HappyAbsSyn65 CaseAlt
  | HappyAbsSyn67 [CaseAlt]
  | HappyAbsSyn69 Pattern
  | HappyAbsSyn75 [Pattern]
type HappyState :: * -> * -> *
newtype HappyState b c
  = HappyState (Int
                -> Int -> b -> HappyState b c -> [HappyState b c] -> c)
type LIdent :: *
newtype LIdent = LIdent ((Int, Int), String)
type Number :: *
newtype Number = Number ((Int, Int), String)
type Pattern :: *
data Pattern
  = PVar LIdent
  | PInt Number
  | PAny
  | PListNil
  | PCons UIdent [Pattern]
  | PListCons Pattern Pattern
type Tok :: *
data Tok
  = TS !String !Int
  | TL !String
  | TI !String
  | TV !String
  | TD !String
  | TC !String
  | T_UIdent !String
  | T_LIdent !String
  | T_Number !String
  | T_BinOp !String
  | T_BinOp7P !String
  | T_BinOp6P !String
  | T_BinOp5P !String
  | T_BinOp4P !String
type Token :: *
data Token
  = PT Language.LBNF.Compiletime.Posn Tok
  | Err Language.LBNF.Compiletime.Posn
type TypeName :: *
data TypeName = TNLit UIdent | TNList TypeName
type UIdent :: *
newtype UIdent = UIdent ((Int, Int), String)
replace :: Eq a => a -> a -> [a] -> [a]
clean :: [Char] -> [Char]
parseExpr :: String -> Expr
parseExprSafe :: String -> Either String Expr
-}