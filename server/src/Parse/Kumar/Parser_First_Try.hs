{-# OPTIONS_GHC -dno-suppress-type-signatures -ddump-splices #-}

{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}


module Parse.Kumar.Parser_First_Try where
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


bnfc [lbnf|


EVar.            Expr ::= Ident ;

SimpleDefAss.    SimpleDef ::= Ident "=" Expr ;

(:[]).           [SimpleDef] ::= SimpleDef ;
(:).             [SimpleDef] ::= SimpleDef "and" [SimpleDef] ;

ELet.            Expr ::= "let" [SimpleDef] "in" Expr ;

RecDefAss.       RecDef ::= "rec" Ident Ident "=" Expr ;

(:[]).           [RecDef] ::= RecDef ;
(:).             [RecDef] ::= RecDef "and" [RecDef] ;

ERec.            Expr ::= "let" [RecDef] "in" Expr ;


TLit.            TypeExpr3 ::= Ident ;
TArr.            TypeExpr2 ::= TypeExpr2 "->" TypeExpr3 ;
TApp.            TypeExpr1 ::= TypeExpr1 TypeExpr2 ;

coercions TypeExpr 3 ;

[].              [TypeExpr] ::= ;
(:).             [TypeExpr] ::= TypeExpr [TypeExpr] ;


[].              [Ident] ::= ;
(:).             [Ident] ::= Ident [Ident] ;

ConstructorDef.  ConstructorDef ::= Ident [TypeExpr] ;

(:[]).           [ConstructorDef] ::= ConstructorDef ;
(:).             [ConstructorDef] ::= ConstructorDef "|" [ConstructorDef] ;


DataDefAss.      DataDef ::= "data" Ident "=" [ConstructorDef] ;

(:[]).           [DataDef] ::= DataDef ;
(:).             [DataDef] ::= DataDef "and" [DataDef] ;

EData.           Expr ::= [DataDef] "in" Expr ;

entrypoints Expr, TypeExpr, Ident, DataDef, ConstructorDef, RecDef, SimpleDef ;


  |]


