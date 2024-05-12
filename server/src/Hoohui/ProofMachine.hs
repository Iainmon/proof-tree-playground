module Hoohui.ProofMachine where

import Logic.Unification.Basic hiding (empty,ppTerm)
import qualified Logic.Unification.Basic as U

import Logic.Proof hiding (prove',prove,proofs)

-- import Control.Monad.Branch
import Control.Monad.State
import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad (guard)

-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
import Data.Map (Map)
import qualified Data.Map as Map

import Hoohui.Types
-- import Control.Monad.MonadStatePlus
-- import Data.Functor.Identity (Identity(..))
-- import Control.Monad.ListT
import Control.Monad.Backtrack
import Control.Monad.State.LocalGlobal


data ProofState = ProofState 
  { substState :: HSubst
  , metaVarCount :: Int
  , metaVars :: [Name]
  , depthCounter :: Int
  } deriving (Show)

type KnowledgeBase = Map HTerm HProof

data GlobalState = GlobalState
  { knowledgeBase :: KnowledgeBase
  , cacheHits :: Int
  } deriving (Show)

-- type ProofMachine a = BrancherT GlobalState ProofState Identity a
-- type ProofMachine a = ForkT GlobalState ProofState a
type ProofMachine a = Backtr GlobalState ProofState a

type ProofResults a = [((a,ProofState),GlobalState)]

runProofMachine :: ProofMachine a -> ProofResults a
runProofMachine m = let ~(ls,g) = manyResults 1 m emptyPS emptyGS in map (\(a,s) -> ((a,s),g)) ls
-- runProofMachine m = let ~(ls,_) = runForkT (forkTake 1 m') emptyGS emptyPS in map fst ls
--   where m' = do
--                 a <- m
--                 s <- localGet
--                 -- g <- globalGet
--                 return ((a,s),emptyGS)
-- runProofMachine m = let ~(ls,_) = runIdentity $ (runStateT (runListT (runStateT (runBrancherT m') emptyPS)) emptyGS) in map fst ls
--   where m' = do
--                 a <- m
--                 s <- localGet
--                 g <- globalGet
--                 return ((a,s),g)
-- runProofMachine m = case flip (runBrancher m) emptyGS emptyPS of
--   ((l:_),g) -> [(l,g)] -- let ~(lcs,g) = runBrancher m emptyPS emptyGS in map (\(a,s) -> ((a,s),g)) lcs
--   _ -> []
emptyS :: Subst v
emptyS = U.empty

emptyPS :: ProofState
emptyPS = ProofState emptyS 0 [] 0

emptyGS :: GlobalState
emptyGS = GlobalState Map.empty 0

putSubst :: HSubst -> ProofMachine ()
putSubst s = local $ modify (\pst -> pst {substState = s})

getSubst :: ProofMachine HSubst
getSubst = local $ gets substState

modifySubst :: (HSubst -> HSubst) -> ProofMachine ()
modifySubst f = do
  s <- local $ gets substState
  putSubst (f s)

newMetaVar :: Name -> ProofMachine Name
newMetaVar n = do
  local $ modify (\pst -> pst {metaVarCount = metaVarCount pst + 1})
  i <- local $ gets metaVarCount
  let v = n ++ "_{" ++ show i ++ "}"
  local $ modify (\pst -> pst {metaVars = v : metaVars pst})
  return v

incMetaVar :: ProofMachine Int
incMetaVar = do
  n <- local $ gets metaVarCount
  local $ modify (\pst -> pst {metaVarCount = n + 1})
  local $ gets metaVarCount

incDepth :: ProofMachine ()
incDepth = local $ modify (\pst -> pst {depthCounter = depthCounter pst + 1})

newProof :: HTerm -> HProof -> ProofMachine ()
newProof t _ | hasFreeVars t = return ()
newProof t p = do
  global $ modify (\gs -> gs {knowledgeBase = Map.insert t p (knowledgeBase gs)})


checkMaxDepth :: Int -> ProofMachine ()
checkMaxDepth d = do
  i <- local $ gets depthCounter
  -- i <- gets metaVarCount
  guard $ i < d