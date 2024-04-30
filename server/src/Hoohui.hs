{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Hoohui where


import Logic.Unification.Basic hiding (empty,ppTerm)
import qualified Logic.Unification.Basic as U

import Logic.Proof hiding (prove',prove,proofs)
import Text.Latex

import Data.List (intercalate)

import Hoohui.Parser 
  ( parseTerm
  , parseTerm'
  , parseRuleSystem
  , parseAll
  , mixFixSpecs
  , toInfix
  , MixFixSpec(..)
  , MixFixPattern
  , MixFixPatPart(..))

import Control.Monad.Branch
import Control.Monad.State
import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad (guard)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.Mem (performGC)

import Hoohui.ProofMachine
import Hoohui.Prover
import Hoohui.Types



parseJudgement :: String -> String -> BEntailJ
parseJudgement source query = mkEntailJ (parseRuleSystem source) (parseTerm' source query) 

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



type FMTJ = (RuleName,HTerm,HTerm,[MixFixSpec])

instance Latex FMTJ where
  latex (rn,t1,t2,mfSpec) = case freeVars t1 of 
    [] -> latexRN rn ++ latexJ t2
    _  -> latexRN rn ++ latexJ t1 ++ " \\sim " ++ latexJ t2
    where latexJ t | Just (mfp,s) <- toInfix t mfSpec = intercalate "\\texttt{ }" $ map (latexInfix s) mfp
          latexJ (Term f []) = "\\texttt{" ++ ru f ++ "}"
          latexJ (Term f ts) = "\\texttt{" ++ ru f ++ "}" ++ "(" ++ intercalate ", " (map latex ts) ++ ")"
          latexJ j = latex j
          latexRN rn = "\\vdash_{" ++ "\\texttt{" ++ ru rn ++ "}}" -- "[\\texttt{[" ++ ru rn ++ "}] \\vdash "
          latexInfix _ (MFPSym s) = "\\texttt{" ++ ru s ++ "}"
          latexInfix _ (MFPTxt t) = "\\texttt{" ++ ru t ++ "}"
          latexInfix s (MFPVar v) | Just t <- Map.lookup v s = latex t
                                  | otherwise = "\\mathcal{" ++ v ++ "}"


-- prove' (EntailJ rs g r s) = fmap (\(rn,_,j) -> mkEntailJ rs j) pf
--   where pf = fst $ head $ flip run emptyS $ myProofs rs g -- prove rs g

provePM' (EntailJ rs g r s) = fmap (\(rn,_,j) -> mkEntailJ rs j) pf
  where pf = fst $ head proofs
        proofs = flip run emptyPS $ proofsPM rs g

maybeHead (x:_) = Just x
maybeHead _ = Nothing

maybeRunProofs rs g = let !x = maybeHead $ run (proofsPM rs g) emptyPS in x

proveIO :: EntailJ Name -> String -> String -> IO (Proof FMTJ)
proveIO j@(EntailJ rs g r s) src q
  = case maybeRunProofs rs g of
      Just (pf,pst) -> do
        performGC
        -- let (pf,pst) = proofs
        -- print $ map (fmtHJudgement . conclusion) $ Map.elems $ knowledgeBase pst
        print $ Map.size $ knowledgeBase pst
        print $ metaVarCount pst
        print $ depthCounter pst
        let fv = freeVars g
        let formattedProof = fmap (\(rn,j,j') -> (rn,if any (`elem`fv) (freeVars j) then j else j',j',mfSpecs)) pf
        performGC
        putStrLn "done!"
        return formattedProof -- fmap (\(rn,j) -> mkEntailJ rs j) pf
      _ -> do
        return $ Leaf ("*",g,Term "no proof found" [],[]) -- Leaf (j { goal = Term "no proof found" [] })
  where mfSpecs = let (_,_,parseState) = parseAll src q in mixFixSpecs parseState

test s = proveIO (mkEntailJ rs (parseTerm s))





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


-- qmProofs e = map fmtHProof $ fmap fst $ flip run emptyS (myProofs rs (parseTerm e))
--   where rs = parseRuleSystem $ unlines 
--             [ "[less_than_nec]   less_than(Z,S(Z)) -: ;"
--             , "[less_than_base]  less_than(S({N}),S({M})) -: less_than({N},{M});"
--             , "[less_than_trans] less_than({N},{M}) -: less_than({N},{K}), less_than({K},{M}) ;"
--             ]


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
