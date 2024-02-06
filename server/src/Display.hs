module Display where

import Latex
import Logic.Proof
import Kumar
import Operational
import Data.List (intercalate)

pt' :: String -> Proof EvalJ
pt' = mkProof . parseExpr

instance Latex Expr where
  latex (EVar x) = x
  latex (EInt n) = show n
  latex (ECon n) = n
  latex (EList es) = "[" ++ intercalate "," (map latex es) ++ "]"
  latex (EFun x e) = "fun " ++ x ++ " -> " ++ latex e
  latex (ELet (DSimp x e1) e2) = "let " ++ x ++ " = " ++ latex e1 ++ " in " ++ latex e2
  latex (ERec ds e2) = "let " ++ intercalate " and " (map latex ds) ++ " in " ++ latex e2
  latex (ECase e alts) = "case " ++ latex e ++ " of \\{ " ++ intercalate " ; " (map (\(p,e) -> latex p ++ " -> " ++ latex e) alts) ++ " \\}"
  latex (EApp e1 e2) | simple e2 = latex e1 ++ " " ++ latex e2
    where simple (EVar _) = True
          simple (ECon _) = True
          simple (EInt _) = True
          simple (EList _) = True
          simple (EBinOp _ _ _) = True
          simple _ = False
  latex (EApp e1 e2) = latex e1 ++ " (" ++ latex e2 ++ ")"
  latex (EBinOp e1 op e2) = "(" ++ latex e1 ++ " " ++ op ++ " " ++ latex e2 ++ ")"
  latex (EIf e1 e2 e3) = "if " ++ latex e1 ++ " then " ++ latex e2 ++ " else " ++ latex e3
instance Latex Decl where
  latex (DSimp x e) = x ++ " = " ++ latex e
  latex (DRec f x e) = "rec " ++ f ++ " " ++ x ++ " = " ++ latex e

instance Latex Pattern where
  latex PAny = "\\_"
  latex (PVar x) = x
  latex (PCons n ps) = n ++ " " ++ intercalate " " (map latex ps)


latexEnv :: Env -> String
latexEnv rho = "\\{ " ++ intercalate "," (map (\(x,v) -> "\\texttt{" ++ x ++ "} \\mapsto \\texttt{" ++ latex v ++ "}") rho) ++ " \\}"

latexList :: Value -> String
latexList (VCon "[]" []) = ""
latexList (VCon ":" [v1,VCon "[]" []]) = latex v1
latexList (VCon ":" [v1,v2]) = show v1 ++ "," ++ latexList v2
latexList v = latex v

instance Latex Value where
  latex (VCon n []) = n
  latex v@(VCon ":" [v1,v2]) = "[" ++ latexList v ++ "]"
  latex (VCon n vs) = n ++ " " ++ intercalate " " [if simple v then latex v else "(" ++ latex v ++ ")" | v <- vs]
    where simple (VCon n []) = True
          simple (VCon ":" _) = True
          simple _ = False
  latex (VClosure x e rho) = "$($" ++ "\\textsf{\\textbf{closure}} " ++ x ++ "$\\to$" ++ latex e ++ "$)$"-- ++ " " ++ latexEnv rho


instance Latex EvalJ where
  latex (EvalJ r e v) = {-latexEnv r ++-} "\\{\\ldots\\} \\vdash \\texttt{" ++ latex e ++ "} \\Rightarrow \\texttt{" ++ latex v ++ "}"

