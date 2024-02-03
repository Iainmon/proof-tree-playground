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




import Language.LBNF
import Data.List (lines)


bnfc [lbnf|

position token UIdent (upper (letter | digit | '_')*);
position token LIdent (lower (letter | digit | '_')*);
position token Number (('-' (digit+)) | (digit+));
position token BinOp ('+' | '-' | '*' | '/' | '=' | '<' | '>' | ':' | '.')+;
position token BinOp7P ('*' | '/');
position token BinOp6P ('+' | '-');
position token BinOp5P (':' | ('+' '+'));
position token BinOp4P ((('<' | '>' | '=') ('=')) | '<' | '>' );

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
EBinOp5.         Expr5 ::= Expr5 BinOp5P Expr6 ;
EBinOp4.         Expr4 ::= Expr4 BinOp4P Expr5 ;

coercions Expr 10 ;
separator Expr "," ;

DSimp.           Decl ::= LIdent "=" Expr ;
DRec.            Decl ::= "rec" LIdent LIdent "=" Expr ;
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

PVar.           Pattern2 ::= LIdent ;
PInt.           Pattern2 ::= Number ;
PAny.           Pattern2 ::= "_" ;
PListNil.       Pattern2 ::= "[" "]" ;
PCons.          Pattern1 ::= UIdent [Pattern2];
PListCons.      Pattern  ::= Pattern1 ":" Pattern ;
coercions Pattern 2 ;
separator Pattern2 "" ;

entrypoints Expr, Decl, CaseAlt, Pattern ;


  |]

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

clean ('\\':'n':s) = ' ' : clean s
clean (c:s) = c : clean s
clean [] = []

parseExpr :: String -> Expr
parseExpr s = case pExpr (myLexer (clean s)) of
  Ok e -> e
  Bad s -> error s

parseExprSafe :: String -> Either String Expr
parseExprSafe s = case pExpr (myLexer (clean s)) of
  Ok e -> Right e
  Bad err -> Left err
