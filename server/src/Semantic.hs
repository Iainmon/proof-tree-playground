module Semantic where

import Kumar
import Data.Maybe (fromJust)

eval :: Env -> Expr -> Value
eval env (EVar n) = fromJust (lookup n env)
eval env (EInt n) = VCon (show n) [] [TNLit "Int"]
eval env (EChar c) = VCon (show c) [] [TNLit "Char"]
eval env (ECon n) = fromJust (lookup n env)
eval env (ELet (d:_) e) = eval (evalDecl env d ++ env) e
eval env (EApp e1 e2) | VCon n vs (_:ts) <- eval env e1 = VCon n (vs ++ [eval env e2]) ts

  -- VClosure x e' env' -> eval ((x, eval env e2) : env') e'
  -- _ -> error "not a closure"
eval env e = error $ show env ++ " " ++ show e

evalDecl :: Env -> Decl -> Env
evalDecl env (DSimp n e) = [(n, eval env e)]
evalDecl env (DRec f x e') = [(f, VClosure x (ELet [DRec f x e'] e') env)]
evalDecl env (DType t cs) = [(n, VCon n [] (ns ++ [TNLit t])) | (n,ns) <- cs]

ex = parseExpr "let data Maybe = Just Int | Nothing in Just 1"