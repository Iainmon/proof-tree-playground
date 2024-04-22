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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.Mem (performGC)


freeVars :: Term v -> [v]
freeVars (Var v) = [v]
freeVars (Term _ ts) = concatMap freeVars ts


parseJudgement :: String -> String -> BEntailJ
parseJudgement source query = mkEntailJ (parseRuleSystem source) (parseTerm query) 

latexNumber :: Term Name -> Maybe Int
latexNumber (Term f [t]) | f == "S" || f == "succ" = do
  n <- latexNumber t
  return (n + 1)
latexNumber (Term f []) | f == "Z" || f == "zero" = Just 0
latexNumber _ = Nothing

instance Latex (Term Name) where
  latex (Var v) = "\\mathcal{" ++ v ++ "}"
  latex t | Just s <- latexNumber t = "\\texttt{" ++ show s ++ "}"
  latex (Term f []) = "\\texttt{" ++ ru f ++ "}"
  latex (Term f ts) = "\\texttt{" ++ ru f ++ "(}" ++ "" ++ intercalate ", " (map latex ts) ++ "\\texttt{)}"

instance Latex (EntailJ Name) where
  -- latex j = latex (goal j)
  latex j = case goal j of
    Term f [] -> "\\textsf{" ++ ru f ++ "}"
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



type FMTJ = (RuleName,HTerm,HTerm)

instance Latex FMTJ where
  latex (rn,t1,t2) = case freeVars t1 of 
    [] -> latexRN rn ++ latexJ t2
    _  -> latexRN rn ++ latexJ t1 ++ " \\sim " ++ latexJ t2
    where latexJ (Term f []) = "\\texttt{" ++ ru f ++ "}"
          latexJ (Term f ts) = "\\texttt{" ++ ru f ++ "}" ++ "(" ++ intercalate ", " (map latex ts) ++ ")"
          latexJ j = latex j
          latexRN rn = "\\vdash_{" ++ "\\texttt{" ++ ru rn ++ "}}" -- "[\\texttt{[" ++ ru rn ++ "}] \\vdash "

prove' (EntailJ rs g r s) = fmap (\(rn,_,j) -> mkEntailJ rs j) pf
  where pf = fst $ head $ flip run emptyS $ myProofs rs g -- prove rs g

provePM' (EntailJ rs g r s) = fmap (\(rn,_,j) -> mkEntailJ rs j) pf
  where pf = fst $ head proofs
        proofs = flip run emptyPS $ proofsPM rs g

maybeHead (x:_) = Just x
maybeHead _ = Nothing


proveIO :: EntailJ Name -> IO (Proof FMTJ)
proveIO j@(EntailJ rs g r s)
  = let proofs = flip run emptyPS $ proofsPM rs g in
      case maybeHead proofs of
        Just (pf,pst) -> do
          performGC
          -- let (pf,pst) = proofs
          -- print $ map (fmtHJudgement . conclusion) $ Map.elems $ knowledgeBase pst
          print $ Map.size $ knowledgeBase pst
          print $ metaVarCount pst
          print $ depthCounter pst
          let fv = freeVars g
          let formattedProof = fmap (\(rn,j,j') -> (rn,if any (`elem`fv) (freeVars j) then j else j',j')) pf
          performGC
          return formattedProof -- fmap (\(rn,j) -> mkEntailJ rs j) pf
        _ -> do
          return $ Leaf ("*",g,Term "no proof found" []) -- Leaf (j { goal = Term "no proof found" [] })

test s = proveIO (mkEntailJ rs (parseTerm s))

type HTerm = Term Name
type HSubst = Subst Name
type HRule = Rule Name
type HRuleSystem = RuleSystem Name

type RuleName = Name
type HJudgement = (RuleName,HTerm,HTerm)
type HProof = Proof HJudgement

data ProofState = ProofState 
  { substState :: HSubst
  , metaVarCount :: Int
  , metaVars :: [Name]
  , knowledgeBase :: Map HTerm HProof
  , depthCounter :: Int
  } deriving (Show)

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
newProof t _ | not (null (freeVars t)) = return ()
newProof t p = do
  kb <- gets knowledgeBase
  modify (\pst -> pst {knowledgeBase = Map.insert t p kb})

instantiateRules :: HRuleSystem -> ProofMachine HRuleSystem
instantiateRules = instantiatePM-- = gets substState >>= return . instantiate r

checkMaxDepth :: Int -> ProofMachine ()
checkMaxDepth d = do
  i <- gets depthCounter
  -- i <- gets metaVarCount
  guard $ i < d

matchingRules :: HRuleSystem -> HTerm -> ProofMachine HRule
matchingRules rs t = do
  -- t <- gets substState >>= return . (t' <.)
  incDepth
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
  let s' = unifyOne c t
  let group = map (apply s') (premisesR rule)
  pfs <- sequence (map (proofsPMCached rs) group)
  s'' <- gets substState
  putSubst (s'' <.> s')
  s <- gets substState
  let j = (rn,t,t <. s)
  let proof = Proof j pfs
  newProof (t <. s) proof
  return proof

proofsPMCached :: HRuleSystem -> HTerm -> ProofMachine HProof
proofsPMCached rs t' = do
  checkMaxDepth 100000 -- 1000000
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
  return $ Proof (rn,t,(t <. s)) pfs



data HJ = HJ RuleName HTerm

instance Show HJ where
  show (HJ rn t) = "[" ++ rn ++ "]" ++ " :- " ++ ppTerm t

fmtHJudgement :: HJudgement -> HJ
fmtHJudgement (rn,_,t) = HJ rn t

fmtHProof :: HProof -> Proof HJ
fmtHProof = fmap fmtHJudgement

ppHProof :: HProof -> IO ()
ppHProof = print . fmtHProof


rs = parseRuleSystem $ unlines 
      [ "[less_than_nec]   less_than(0,1) -: ;"
      , "[less_than_base]  less_than(succ({N}),succ({M})) -: less_than({N},{M});"
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
