module Hoohui.Types where


import Logic.Unification.Basic hiding (empty,ppTerm)
import qualified Logic.Unification.Basic as U
import Logic.Proof hiding (prove',prove,proofs)

type HTerm = Term Name
type HSubst = Subst Name
type HRule = Rule Name
type HRuleSystem = RuleSystem Name

type RuleName = Name
type HJudgement = (RuleName,HTerm,HTerm)
type HProof = Proof HJudgement

freeVars :: Term v -> [v]
freeVars (Var v) = [v]
freeVars (Term _ ts) = concatMap freeVars ts

hasFreeVars :: Term v -> Bool
hasFreeVars (Var _) = True
hasFreeVars (Term _ ts) = any hasFreeVars ts