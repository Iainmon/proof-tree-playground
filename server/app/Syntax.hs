module Syntax where

import Logic.Proof
import Data.List (intercalate)

data Expr
  = Lit Int
  | Pair Expr Expr
  deriving (Eq,Show)


data Type
  = TInt
  | TPair Type Type
  deriving (Eq,Show)

data TJ
  = TJ Expr Type
  deriving (Eq,Show)


inferType :: Expr -> Type
inferType (Lit _) = TInt
inferType (Pair e1 e2) = TPair (inferType e1) (inferType e2)

pt :: Expr -> Maybe (Proof TJ)
pt e = prove (TJ e (inferType e))

pt' :: Expr -> Proof TJ
pt' e = case pt e of
  Just p -> p
  Nothing -> error "no proof"


instance Explain TJ where
  premises (TJ (Pair e1 e2) (TPair t1 t2)) = [[TJ e1 t1, TJ e2 t2]]
  premises (TJ (Lit _) TInt) = [[]]
  premises _                 = []


class Latex a where
  latex :: a -> String

instance Latex Expr where
  latex (Lit n) = show n
  latex (Pair e1 e2) = "(" ++ latex e1 ++ "," ++ latex e2 ++ ")"

instance Latex Type where
  latex TInt = "\\mathbb{N}"
  latex (TPair TInt TInt) = "\\mathbb{N} \\times \\mathbb{N}"
  latex (TPair t1 t2) = "(" ++ latex t1 ++ "\\times " ++ latex t2 ++ ")"

instance Latex TJ where
  latex (TJ e t) = latex e ++ " : " ++ latex t

-- toJson :: Proof TJ -> String
-- toJson (Proof j ps) = "{ \"conclusion\": \"" ++ latex j ++ "\", \"premises\": [" ++ intercalate "," (map toJson ps) ++ "] }"
