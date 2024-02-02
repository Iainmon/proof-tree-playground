module Semantic where

import Kumar
import Data.Maybe (fromJust)
import Control.Monad (zipWithM)
import Data.List (nub)




eval :: Env -> Expr -> Value
{- Num -}
eval env (EInt n) = VCon (show n) []
{- Con -}
eval env (ECon n) = VCon n []
{- Var -}
eval env (EVar x) = fromJust (lookup x env)
{- Fun -}
eval env (EFun x e) = VClosure x e env
{- App -}
eval env (EApp e1 e2) | VClosure x e3 env' <- eval env e1 = eval ((x, eval env e2) : env') e3
{- AppCon -}
eval env (EApp e1 e2) | VCon n vs <- eval env e1 = VCon n (vs ++ [eval env e2])
{- Let -}
eval env (ELet (DSimp x e1) e2) = eval ((x, eval env e1) : env) e2
{- LetRec (incomplete) -}
eval env (ERec [DRec f x e1] e2) = eval ((f, VClosure x (ERec [DRec f x e1] e1) env) : env) e2
{- Case -}
eval env (ECase e alts) | v <- eval env e, Just (envi,ei) <- firstMatch v alts = eval (envi ++ env) ei
{- BuiltInOp (incomplete) -}
eval env (EBinOp e1 op e2) | BuiltInOp <- op, Just v <- evalBuiltIn op (eval env e1) (eval env e2) = v

{- List -}

{- Desugar -}
eval env (EList []) = VCon "[]" []
eval env (EList (e:es)) = eval env (EApp (EApp (ECon ":") e) (EList es))

eval env e = error ("eval: " ++ show env ++ " |- " ++ show e ++ " -> ?")


firstMatch :: Value -> [CaseAlt] -> Maybe (Env,Expr)
firstMatch v [] = Nothing
firstMatch v ((p,e):as) = case match v p of
  Just env -> Just (env,e)
  Nothing  -> firstMatch v as


match :: Value -> Pattern -> Maybe Env
match v PAny     = Just []
match v (PVar x) = Just [(x,v)]
match (VCon n vs) (PCons n' ps) | n == n', length vs == length ps 
  = do envs <- zipWithM match vs ps
       let env = concat envs
       if functional env 
        then Just env 
        else Nothing
match _ _ = Nothing

functional :: Eq a => [(a,b)] -> Bool
functional f = length dom == length (nub dom)
  where dom = map fst f


builtInNum :: BinOp -> Maybe (Int -> Int -> Int)
builtInNum BOAdd = Just (+)
builtInNum BOSub = Just (-)
builtInNum BOMul = Just (*)
builtInNum BODiv = Just div

evalBuiltIn :: BinOp -> Value -> Value -> Maybe Value
evalBuiltIn op v1 v2 | Just f <- builtInNum op
 = do n1 <- numValue v1
      n2 <- numValue v2
      return (VCon (show (f n1 n2)) [])


ex0 = parseExpr "1"
ex1 = parseExpr "let x = 1 in Left x"
ex2 = parseExpr "let x = Just 1 in let y = Nothing in case x of { Just z -> Just y ; Nothing -> 0 }"
ex3 = parseExpr "[case [1,2] of { [] -> 0 ; x:xs -> x },[2,3]]"
ex4 = parseExpr "[1,2]"
ex6 = parseExpr "let rec f x = x * x in f 2"