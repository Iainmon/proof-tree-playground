{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Hoohui where


import Logic.Unification.Basic hiding (empty,ppTerm)
import qualified Logic.Unification.Basic as U

import Logic.Proof hiding (prove',prove,proofs)
import Text.Latex

import Data.List (intercalate)

import Hoohui.Parser (parseTerm, parseRuleSystem)

import Control.Monad.Branch
import Control.Monad.State
import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad (guard)

import Data.Map (Map)
import qualified Data.Map as Map



parseJudgement :: String -> String -> BEntailJ
parseJudgement source query = mkEntailJ (parseRuleSystem source) (parseTerm query) 

latexNumber :: Term Name -> Maybe Int
latexNumber (Term "S" [t]) = do
  n <- latexNumber t
  return (n + 1)
latexNumber (Term "Z" []) = Just 0
latexNumber _ = Nothing

instance Latex (Term Name) where
  latex (Var v) = "\\mathcal{" ++ v ++ "}"
  latex t | Just s <- latexNumber t = "\\texttt{" ++ show s ++ "}"
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

ppTerm :: Term Name -> String
ppTerm (Var v) = "{" ++ v ++ "}"
ppTerm (Term f []) = f
ppTerm (Term f ts) = f ++ "(" ++ intercalate ", " (map ppTerm ts) ++ ")"



prove' (EntailJ rs g r s) = fmap (\(rn,j) -> mkEntailJ rs j) pf
  where pf = fst $ head $ flip run emptyS $ myProofs rs g -- prove rs g

provePM' (EntailJ rs g r s) = fmap (\(rn,j) -> mkEntailJ rs j) pf
  where pf = fst $ head proofs
        proofs = flip run emptyPS $ proofsPM rs g

proveIO :: EntailJ Name -> IO (Proof (EntailJ Name))
proveIO (EntailJ rs g r s) = do
  let proofs = flip run emptyPS $ proofsPM rs g
  let (pf,pst) = head proofs
  -- print $ map (fmtHJudgement . conclusion) $ Map.elems $ knowledgeBase pst
  print $ Map.size $ knowledgeBase pst
  print $ metaVarCount pst
  return $ fmap (\(rn,j) -> mkEntailJ rs j) pf


type HTerm = Term Name
type HSubst = Subst Name
type HRule = Rule Name
type HRuleSystem = RuleSystem Name

type RuleName = Name
type HJudgement = (RuleName,HTerm)
type HProof = Proof HJudgement

data ProofState = ProofState 
  { substState :: HSubst
  , metaVarCount :: Int
  , metaVars :: [Name]
  , knowledgeBase :: Map HTerm HProof
  } deriving (Show)

emptyS :: Subst v
emptyS = U.empty

emptyPS :: ProofState
emptyPS = ProofState emptyS 0 [] Map.empty

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

newProof :: HTerm -> HProof -> ProofMachine ()
newProof t p = do
  kb <- gets knowledgeBase
  modify (\pst -> pst {knowledgeBase = Map.insert t p kb})

instantiateRules :: HRuleSystem -> ProofMachine HRuleSystem
instantiateRules = instantiatePM-- = gets substState >>= return . instantiate r

checkMaxDepth :: Int -> ProofMachine ()
checkMaxDepth d = do
  i <- gets metaVarCount
  guard $ i < d

matchingRules :: HRuleSystem -> HTerm -> ProofMachine HRule
matchingRules rs t = do
  -- t <- gets substState >>= return . (t' <.)
  rs' <- instantiateRules rs
  let rules = [r | r@(Rule _ c _) <- rs', Just s <- [safeUnify t c]]
  guard $ not (null rules)
  each rules


proofsPM :: HRuleSystem -> HTerm -> ProofMachine HProof
proofsPM rs t' = do
  t <- gets substState >>= return . (t' <.)
  rule <- matchingRules rs t
  let rn = nameR rule
  let c = conclusionR rule
  let s' = unifyOne t c
  let group = map (apply s') (premisesR rule)
  pfs <- sequence (map (proofsPMCached rs) group)
  s'' <- gets substState
  putSubst (s'' <.> s')
  s <- gets substState
  let j = (rn,t <. s)
  let proof = Proof j pfs
  newProof (snd j) proof
  return proof

proofsPMCached :: HRuleSystem -> HTerm -> ProofMachine HProof
proofsPMCached rs t' = do
  checkMaxDepth 1000000
  t <- gets substState >>= return . (t' <.)
  kb <- gets (Map.lookup t . knowledgeBase)
  case kb of
    Just p -> return p
    Nothing -> proofsPM rs t

type ProofMachine a = Branch ProofState a



instantiatePM :: HRuleSystem -> ProofMachine HRuleSystem
instantiatePM [] = return []
instantiatePM (r:rs) = do
  n <- incMetaVar
  let r' = r {conclusionR = incrementFV n (conclusionR r), premisesR = map (incrementFV n) (premisesR r)}
  rs' <- instantiatePM rs
  return (r':rs')
  where incrementFV n (Var v) = Var (v ++ "_{" ++ show n ++ "}")
        incrementFV n (Term f ts) = Term f (map (incrementFV n) ts)


instantiate :: HRuleSystem -> HSubst -> HRuleSystem
instantiate rs s = go rs (length freeVars)
  where incrementFV n (Var v) = Var (v ++ "_{" ++ show n ++ "}")
        incrementFV n (Term f ts) = Term f (map (incrementFV n) ts)
        freeVars = Map.keys s
        go [] _ = []
        go (r:rs) n = r {conclusionR = incrementFV n (conclusionR r), premisesR = map (incrementFV n) (premisesR r)} : go rs (n+1)



instantiateB :: HRuleSystem -> Branch HSubst HRuleSystem
instantiateB rs = do
  s <- get
  return $ instantiate rs s

myProofs :: HRuleSystem -> HTerm -> Branch HSubst HProof
myProofs rs t' = do
  t <- get >>= return . (t' <.)
  rs' <- instantiateB rs
  let rules = [r | r@(Rule _ c _) <- rs', Just s <- [safeUnify t c]]
  guard $ not (null rules)
  rule <- each rules
  let rn = nameR rule
  let c = conclusionR rule
  let s' = unifyOne t c
  let group = map (apply s') (premisesR rule)
  pfs <- sequence (map (myProofs rs) group)
  s'' <- get
  put (s' <.> s'')
  s <- get
  return $ Proof (rn,(t <. s)) pfs



data HJ = HJ RuleName HTerm

instance Show HJ where
  show (HJ rn t) = "[" ++ rn ++ "]" ++ " :- " ++ ppTerm t

fmtHJudgement :: HJudgement -> HJ
fmtHJudgement (rn,t) = HJ rn t

fmtHProof :: HProof -> Proof HJ
fmtHProof = fmap fmtHJudgement

ppHProof :: HProof -> IO ()
ppHProof = print . fmtHProof


rs = parseRuleSystem $ unlines 
      [ "[less_than_nec]   less_than(Z,S(Z)) -: ;"
      , "[less_than_base]  less_than(S({N}),S({M})) -: less_than({N},{M});"
      , "[less_than_trans] less_than({N},{M}) -: less_than({N},{K}), less_than({K},{M}) ;"
      ]


qmProofs e = map fmtHProof $ fmap fst $ flip run emptyS (myProofs rs (parseTerm e))
  where rs = parseRuleSystem $ unlines 
            [ "[less_than_nec]   less_than(Z,S(Z)) -: ;"
            , "[less_than_base]  less_than(S({N}),S({M})) -: less_than({N},{M});"
            , "[less_than_trans] less_than({N},{M}) -: less_than({N},{K}), less_than({K},{M}) ;"
            ]


{-
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
-}

-- rs = [
--       Rule {nameR = "rule1", conclusionR = Term "friends" [Term "iain" [],Term "kassia" []], premisesR = []}
--       ,Rule {nameR = "rule2", conclusionR = Term "friends" [Term "kassia" [],Term "grace" []], premisesR = []}
--       ,Rule {nameR = "rule3", conclusionR = Term "friends" [Term "grace" [],Term "ron" []], premisesR = []}
--       ,Rule {nameR = "rule4", conclusionR = Term "friends" [Var "X",Var "Y"], premisesR = [Term "friends" [Var "X",Var "Z"],Term "friends" [Var "Z",Var "Y"]]}
--       -- Rule {nameR = "rule5", conclusionR = Term "hasFriends" [Var "X"], premisesR = [Term "friends" [Var "X",Var "Y"]]}
--     ]

-- t = parseTerm "friends(iain,ron)"

{-
qProofS :: Branch HSubst HProof
qProofS = prove rs (parseTerm "less_than(S(Z),S(S(S(Z))))")

-- qProofS = prove rs $ parseTerm "less_than(Z,{K})" -- (parseTerm "less_than(S(Z),S(S(S(Z))))")

qProofs = fmap fst $ flip run emptyS qProofS
-}

-- data RT a b = Root a [RT b b]
-- proofs = flip run emptyS . prove rs
