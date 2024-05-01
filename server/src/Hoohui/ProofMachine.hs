module Hoohui.ProofMachine where

import Logic.Unification.Basic hiding (empty,ppTerm)
import qualified Logic.Unification.Basic as U

import Logic.Proof hiding (prove',prove,proofs)

import Control.Monad.Branch
import Control.Monad.State
import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad (guard)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Hoohui.Types


data ProofState = ProofState 
  { substState :: HSubst
  , metaVarCount :: Int
  , metaVars :: [Name]
  , knowledgeBase :: Map HTerm HProof
  , depthCounter :: Int
  } deriving (Show)


type ProofMachine a = Branch ProofState a


-- runProofMachine :: ProofMachine 

emptyS :: Subst v
emptyS = U.empty

emptyPS :: ProofState
emptyPS = ProofState emptyS 0 [] Map.empty 0

putSubst :: HSubst -> ProofMachine ()
putSubst s = modify (\pst -> pst {substState = s})

getSubst :: ProofMachine HSubst
getSubst = gets substState

modifySubst :: (HSubst -> HSubst) -> ProofMachine ()
modifySubst f = do
  s <- gets substState
  putSubst (f s)

newMetaVar :: Name -> ProofMachine Name
newMetaVar n = do
  modify (\pst -> pst {metaVarCount = metaVarCount pst + 1})
  i <- gets metaVarCount
  let v = n ++ "_{" ++ show i ++ "}"
  modify (\pst -> pst {metaVars = v : metaVars pst})
  return v

incMetaVar :: ProofMachine Int
incMetaVar = do
  n <- gets metaVarCount
  modify (\pst -> pst {metaVarCount = n + 1})
  gets metaVarCount

incDepth :: ProofMachine ()
incDepth = modify (\pst -> pst {depthCounter = depthCounter pst + 1})

newProof :: HTerm -> HProof -> ProofMachine ()
newProof t _ | hasFreeVars t = return ()
newProof t p = do
  kb <- gets knowledgeBase
  modify (\pst -> pst {knowledgeBase = Map.insert t p kb})


checkMaxDepth :: Int -> ProofMachine ()
checkMaxDepth d = do
  i <- gets depthCounter
  -- i <- gets metaVarCount
  guard $ i < d