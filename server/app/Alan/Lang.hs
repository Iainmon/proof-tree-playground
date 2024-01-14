{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Alan.Lang where


import Language.LBNF.Compiletime
import Language.LBNF(lbnf, bnfc)

bnfc [lbnf|
  Ap.  Expr ::= Expr Expr;
  Lam. Expr ::= "\\" Ident "->" Expr;
  Var. Expr ::= Ident;

  TInt. Term ::= Integer;
  TStr. Term ::= Ident;
  TVar. Term ::= "?" Ident;
  TCll. Term ::= Ident "(" [Term] ")";

  [].     [Term] ::= ;
  (:[]).  [Term] ::= Term;
  (:).    [Term] ::= Term "," [Term] ;

|]