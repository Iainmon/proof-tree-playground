module Hoohui.Prover where


import Logic.Unification.Basic hiding (empty,ppTerm)
import qualified Logic.Unification.Basic as U


import Logic.Proof hiding (prove',prove,proofs)

import Control.Monad.Branch
import Control.Monad.State
import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad (guard)
import Control.Monad.MonadStatePlus

-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
import Data.Map (Map)
import qualified Data.Map as Map


import Hoohui.ProofMachine
import Hoohui.Types


matchingRules :: HRuleSystem -> HTerm -> ProofMachine HRule
matchingRules rs t = do
  -- t <- localGets substState >>= return . (t' <.)
  incDepth
  rs' <- instantiateRules rs
  let !rules = [r | r@(Rule _ c ps) <- rs', Just s <- [safeUnify t c], not $ any (=="fail") [f | Term f _ <- ps]]
  guard $ not (null rules)
  -- guard $ not $ any (\r -> or [f == "fail" | Term f _ <- premisesR r]) rules
  each rules


proofsPM :: HRuleSystem -> HTerm -> ProofMachine HProof
proofsPM rs t' = do
  t <- localGets substState >>= return . (t' <.)
  rule <- matchingRules rs t
  let rn = nameR rule
  let c = conclusionR rule
  let s' = unifyOne c t
  let group = map (apply s') (premisesR rule)
  pfs <- sequence (map (proofsPMCached rs) group)
  s'' <- localGets substState
  putSubst (s'' <.> s')
  s <- localGets substState
  let !j = (rn,t,t <. s)
  let !proof = Proof j pfs
  newProof (t <. s) proof
  return proof

proofsPMCached :: HRuleSystem -> HTerm -> ProofMachine HProof
proofsPMCached rs t' = do
  checkMaxDepth 100 -- 1000000
  t <- localGets substState >>= return . (t' <.)
  if hasFreeVars t then proofsPM rs t else do
    kb <- globalGets (Map.lookup t . knowledgeBase)
    case kb of
      Just p -> return p
      Nothing -> proofsPM rs t


instantiateRules :: HRuleSystem -> ProofMachine HRuleSystem
instantiateRules = instantiatePM-- = gets substState >>= return . instantiate r

instantiatePM :: HRuleSystem -> ProofMachine HRuleSystem
instantiatePM [] = return []
instantiatePM (r:rs) = do
  n <- incMetaVar
  let r' = r {conclusionR = incrementFV n (conclusionR r), premisesR = map (incrementFV n) (premisesR r)}
  rs' <- instantiatePM rs
  return (r':rs')
  where incrementFV n (Var v) = Var (v ++ "_{" ++ show n ++ "}")
        incrementFV n (Term f ts) = Term f (map (incrementFV n) ts)


-- instantiate :: HRuleSystem -> HSubst -> HRuleSystem
-- instantiate rs s = go rs (length freeVars)
--   where incrementFV n (Var v) = Var (v ++ "_{" ++ show n ++ "}")
--         incrementFV n (Term f ts) = Term f (map (incrementFV n) ts)
--         freeVars = Map.keys s
--         go [] _ = []
--         go (r:rs) n = r {conclusionR = incrementFV n (conclusionR r), premisesR = map (incrementFV n) (premisesR r)} : go rs (n+1)



-- instantiateB :: HRuleSystem -> Branch HSubst HRuleSystem
-- instantiateB rs = do
--   s <- get
--   return $ instantiate rs s

-- myProofs :: HRuleSystem -> HTerm -> Branch HSubst HProof
-- myProofs rs t' = do
--   t <- get >>= return . (t' <.)
--   rs' <- instantiateB rs
--   let rules = [r | r@(Rule _ c _) <- rs', Just s <- [safeUnify t c]]
--   guard $ not (null rules)
--   rule <- each rules
--   let rn = nameR rule
--   let c = conclusionR rule
--   let s' = unifyOne t c
--   let group = map (apply s') (premisesR rule)
--   pfs <- sequence (map (myProofs rs) group)
--   s'' <- get
--   put (s' <.> s'')
--   s <- get
--   return $ Proof (rn,t,(t <. s)) pfs
