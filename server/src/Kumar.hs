{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Kumar where

import qualified Parse.Kumar.Parser as K
import Logic.Proof
import Data.List (intercalate)
import Text.Read (readMaybe)

{-
data K.Expr
  = K.EVar K.LIdent
  | K.EInt K.Number
  | K.ECon K.UIdent
  | K.EStr String
  | K.EList [K.Expr]
  | K.EBinFn K.BinOp
  | K.EFun K.LIdent K.Expr
  | K.ELet [K.Decl] K.Expr
  | K.ECase K.Expr [K.CaseAlt]
  | K.EIf K.Expr K.Expr K.Expr
  | K.EApp K.Expr K.Expr
  | K.EBinOp8 K.Expr K.BinOp K.Expr
  | K.EBinOp7 K.Expr K.BinOp7P K.Expr
  | K.EBinOp6 K.Expr K.BinOp6P K.Expr
  | K.EBinOp5 K.Expr K.BinOp5P K.Expr
  | K.EBinOp4 K.Expr K.BinOp4P K.Expr

newtype K.BinOp = K.BinOp ((Int, Int), String)
newtype K.UIdent = K.UIdent ((Int, Int), String)

data K.Decl
  = K.DSimp K.LIdent K.Expr
  | K.DRec K.LIdent K.LIdent K.Expr
  | K.DType K.UIdent [K.ConDef]

data K.ConDef = K.ConDef K.UIdent [K.TypeName]
data K.TypeName = K.TNLit K.UIdent | K.TNList K.TypeName

data K.CaseAlt = K.CaseAlt K.Pattern K.Expr

data K.Pattern
  = K.PVar K.LIdent
  | K.PInt K.Number
  | K.PAny
  | K.PListNil
  | K.PCons K.UIdent [K.Pattern]
  | K.PListCons K.Pattern K.Pattern
-}

type Name = String
type BinOp = String

data Expr
  = EVar Name
  | EInt Int
  | ECon Name
  | EList [Expr]
  | EBinFn BinOp
  | EFun Name Expr
  | ELet Decl Expr
  | ERec [Decl] Expr
  | ECase Expr [CaseAlt]
  | EIf Expr Expr Expr
  | EApp Expr Expr
  | EBinOp Expr BinOp Expr
  deriving (Show, Eq)


pattern BuiltInOp :: BinOp
pattern BuiltInOp <- (builtIn -> True)

pattern BOAdd,BOSub,BOMul,BODiv :: BinOp
pattern BOAdd = "+"
pattern BOSub = "-"
pattern BOMul = "*"
pattern BODiv = "/"


builtIn :: BinOp -> Bool
builtIn "+" = True
builtIn "-" = True
builtIn "*" = True
builtIn "/" = True
builtIn _ = False

-- pattern ELetIn :: Name -> Expr -> Expr -> Expr
-- pattern ELetIn x e1 e2 = ELet (DSimp x e1) e2

data TypeName 
  = TNLit Name 
  | TNList TypeName
  | TNCons Name [TypeName]
  deriving Eq

instance Show TypeName where
  show (TNLit x) = x
  show (TNList t) = "[" ++ show t ++ "]"
  show (TNCons x []) = x
  show (TNCons x ts) = x ++ " " ++ intercalate " -> " (map show ts)

type ConDef = (Name,[TypeName])

data Decl
  = DSimp Name Expr
  | DRec Name Name Expr
  | DType Name [ConDef]
  deriving (Show, Eq)

type CaseAlt = (Pattern, Expr)

data Pattern
  = PVar Name
  | PAny
  | PCons Name [Pattern]
  deriving (Show, Eq)

type Env = [(Name, Value)]

data Value
  = VCon Name [Value]
  | VClosure Name Expr Env
  deriving Eq

conValue :: Value -> Maybe Name
conValue (VCon n []) = Just n
conValue _ = Nothing

numValue :: Value -> Maybe Int
numValue v = do n <- conValue v 
                readMaybe n :: Maybe Int

boolValue :: Value -> Maybe Bool
boolValue v = conValue v >>= readMaybe

showListValue :: Value -> String
showListValue (VCon "[]" []) = ""
showListValue (VCon ":" [v1,VCon "[]" []]) = show v1
showListValue (VCon ":" [v1,v2]) = show v1 ++ "," ++ showListValue v2
showListValue v = show v

instance Show Value where
  show (VCon n []) = n
  show v@(VCon ":" [v1,v2]) = "[" ++ showListValue v ++ "]"
  show (VCon n vs) = "(" ++ n ++ " " ++ intercalate " " [show v | v <- vs] ++ ")"
  show (VClosure n e env) = "(clo " ++ n ++ " -> " ++ show e ++ ")"


parseExpr :: String -> Expr
parseExpr s = transExpr (K.parseExpr s)

parseExprSafe :: String -> Either String Expr
parseExprSafe s = case K.parseExprSafe s of
  Left e -> Left e
  Right e -> Right $ transExpr e

transLIdent :: K.LIdent -> Name
transLIdent (K.LIdent (_,x)) = x

transUIdent :: K.UIdent -> Name
transUIdent (K.UIdent (_,x)) = x

transNumber :: K.Number -> Int
transNumber (K.Number (_,n)) = read n :: Int

transBinOp :: K.BinOp -> BinOp
transBinOp (K.BinOp (_,x)) = x

transExpr :: K.Expr -> Expr
transExpr (K.EVar x) = EVar (transLIdent x)
transExpr (K.EInt n) = EInt (transNumber n)
transExpr (K.ECon x) = ECon (transUIdent x)
transExpr (K.EStr x) = EApp (ECon "String") (ECon x)
transExpr (K.EList xs) = EList (map transExpr xs)
transExpr (K.EBinFn bo) = EBinFn (transBinOp bo)
transExpr (K.EFun x e) = EFun (transLIdent x) (transExpr e)
transExpr (K.ELet [d@(K.DSimp x e1)] e2) = ELet (transDecl d) (transExpr e2)
transExpr (K.ELet d e) = ERec (map transDecl d) (transExpr e)
transExpr (K.ECase e as) = ECase (transExpr e) (map transCaseAlt as)
transExpr (K.EIf e1 e2 e3) = EIf (transExpr e1) (transExpr e2) (transExpr e3)
transExpr (K.EApp e1 e2) = EApp (transExpr e1) (transExpr e2)
transExpr (K.EBinOp8 e1 (K.BinOp (_,bo)) e2) = EBinOp (transExpr e1) bo (transExpr e2)
transExpr (K.EBinOp7 e1 (K.BinOp7P (_,bo)) e2) = EBinOp (transExpr e1) bo (transExpr e2)
transExpr (K.EBinOp6 e1 (K.BinOp6P (_,bo)) e2) = EBinOp (transExpr e1) bo (transExpr e2)
transExpr (K.EBinOp5 e1 (K.BinOp5P (_,bo)) e2) = EBinOp (transExpr e1) bo (transExpr e2)
transExpr (K.EBinOp4 e1 (K.BinOp4P (_,bo)) e2) = EBinOp (transExpr e1) bo (transExpr e2)


transDecl :: K.Decl -> Decl
transDecl (K.DSimp x e) = DSimp (transLIdent x) (transExpr e)
transDecl (K.DRec x y e) = DRec (transLIdent x) (transLIdent y) (transExpr e)
transDecl (K.DType x cs) = DType (transUIdent x) (map transConDef cs)

transConDef :: K.ConDef -> ConDef
transConDef (K.ConDef x ts) = (transUIdent x, map transTypeName ts)

transTypeName :: K.TypeName -> TypeName
transTypeName (K.TNLit x) = TNLit (transUIdent x)
transTypeName (K.TNList t) = TNList (transTypeName t)

transCaseAlt :: K.CaseAlt -> CaseAlt
transCaseAlt (K.CaseAlt p e) = (transPattern p, transExpr e)

transPattern :: K.Pattern -> Pattern
transPattern (K.PVar x) = PVar (transLIdent x)
transPattern (K.PInt n) = PCons (show (transNumber n)) []
transPattern K.PAny = PAny
transPattern K.PListNil = PCons "[]" []
transPattern (K.PCons x ps) = PCons (transUIdent x) (map transPattern ps)
transPattern (K.PListCons p1 p2) = PCons ":" (map transPattern [p1,p2])

