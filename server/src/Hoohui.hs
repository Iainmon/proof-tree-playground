{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Hoohui where


import Logic.Unification.Basic hiding (empty)
import qualified Logic.Unification.Basic as U

import Logic.Proof hiding (prove',prove,proofs)
import Text.Latex

import Data.List (intercalate)

import Hoohui.Parser (parseTerm, parseRuleSystem)

import Control.Monad.Branch
import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad (guard)
import qualified Data.Map as Map



parseJudgement :: String -> String -> BEntailJ
parseJudgement source query = mkEntailJ (parseRuleSystem source) (parseTerm query) 

instance Latex (Term Name) where
  latex (Var v) = "\\mathcal{" ++ v ++ "}"
  latex (Term f []) = "\\texttt{" ++ ru f ++ "}"
  latex (Term f ts) = "\\texttt{" ++ ru f ++ "(}" ++ "" ++ intercalate ", " (map latex ts) ++ "\\texttt{)}"

instance Latex (EntailJ Name) where
  -- latex j = latex (goal j)
  latex j = case goal j of
    Term f ts -> "\\textsf{" ++ ru f ++ "}" ++ "(" ++ intercalate ", " (map latex ts) ++ ")"
    _ -> latex (goal j)

ru :: String -> String
ru [] = []
ru ('_':cs) = "\\_" ++ ru cs
ru (c:cs) = c : ru cs

prove' (EntailJ rs g r s) = fmap (\(rn,j) -> mkEntailJ rs j) pf
  where pf = fst $ head $ flip run emptyS $ prove rs g

type HTerm = Term Name
type HSubst = Subst Name
type HRule = Rule Name
type HRuleSystem = RuleSystem Name

type RuleName = Name

type HProof = Proof (RuleName,HTerm)



provePremises :: HRuleSystem -> [(RuleName,HTerm,[HTerm],HSubst)] -> Branch HSubst (RuleName,HTerm,[HProof])
provePremises rs [] = empty
provePremises rs ((rn,c,g,s):gs) = (do {
  put s;
  ps <- proveGroup rs (map (apply s) g); s' <- get; return (rn,c <. s',ps) }) <|> provePremises rs gs

proveGroup :: HRuleSystem -> [HTerm] -> Branch HSubst [HProof]
proveGroup rs [] = return []
proveGroup rs (t:ts) = do
  p <- prove rs t
  s <- get
  ps <- proveGroup rs (map (<.s) ts)
  s' <- get
  put (s <.> s')
  return (p:ps)

prove :: HRuleSystem -> HTerm -> Branch HSubst HProof
prove rs t = do
  s <- get
  let rs' = instantiate rs s
  let applicableGroups = [(rn,c <. s <. s', map (apply s') ps,s') | (Rule rn c ps) <- rs', Just s' <- [safeUnify (t <. s) (c <. s)]]
  (rn,c,ps) <- provePremises rs applicableGroups
  s' <- get
  put (s' <.> s)
  -- guard $ c <. s <. s' == t <. s <. s'
  return $ Node (rn,t <. s <. s') ps
  -- (rn,g,s) <- provePremises rs applicableGroups
  -- pfs <- foldr ((<|>) . proveGroup rs) (return []) gs
  -- return $ Node (rn,t) pfs
    
  -- -- where applicableGroups = undefined

instantiate :: HRuleSystem -> HSubst -> HRuleSystem
instantiate rs s = go rs (length freeVars)
  where incrementFV n (Var v) = Var (v ++ "_{" ++ show n ++ "}")
        incrementFV n (Term f ts) = Term f (map (incrementFV n) ts)
        freeVars = Map.keys s
        go [] _ = []
        go (r:rs) n = r {conclusionR = incrementFV n (conclusionR r), premisesR = map (incrementFV n) (premisesR r)} : go rs (n+1)


rs = [
      Rule {nameR = "rule1", conclusionR = Term "friends" [Term "iain" [],Term "kassia" []], premisesR = []}
      ,Rule {nameR = "rule2", conclusionR = Term "friends" [Term "kassia" [],Term "grace" []], premisesR = []}
      ,Rule {nameR = "rule3", conclusionR = Term "friends" [Term "grace" [],Term "ron" []], premisesR = []}
      ,Rule {nameR = "rule4", conclusionR = Term "friends" [Var "X",Var "Y"], premisesR = [Term "friends" [Var "X",Var "Z"],Term "friends" [Var "Z",Var "Y"]]}
      -- Rule {nameR = "rule5", conclusionR = Term "hasFriends" [Var "X"], premisesR = [Term "friends" [Var "X",Var "Y"]]}
    ]

t = parseTerm "friends(iain,ron)"
emptyS = U.empty

proofs = flip run emptyS . prove rs

