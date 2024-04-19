{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Playground where

import Control.Monad.Branch

import Control.Monad.State
import Control.Monad.Trans

import Control.Applicative ( Alternative(empty, (<|>)) )

import Logic.Proof (
  Proof(..),
  pattern Proof,
  pattern Leaf,
  Explain(..),
  proofs,
  premises)
import Data.List (transpose)

run :: Int -> Branch Int a -> [(a,Int)]
run = flip runBranch

increment :: Branch Int Int
increment = do
  s <- get
  put (s + 1)
  return s

copy :: Int -> Branch s ()
copy n = parallel (replicate n ((),))
-- copy 0 = return ()
-- copy n = do
--   return () <|> copy (n-1)

parallel :: [s -> (a,s)] -> Branch s a
parallel [] = empty
parallel (f:fs) = do
  s <- get
  let (a,s') = f s
  put s'
  return a <|> parallel fs

concurrent :: (s -> a -> [(b,s)]) -> Branch s a -> Branch s b
concurrent f b = branch $ \s -> do (a,s') <- runBranch b s; f s' a



sandbox :: Branch s a -> Branch s (a,s)
sandbox b = do
  s0 <- get
  a <- b
  s1 <- get
  put s0
  return (a,s1)

sandboxMany :: [Branch s a] -> Branch s [(a,s)]
sandboxMany [] = return []
sandboxMany (b:bs) = do
  x <- sandbox b
  xs <- sandboxMany bs
  return (x:xs)

awaitAll :: Branch s a -> Branch s [(a,s)]
awaitAll b = do
  s0 <- get
  return $ runBranch b s0

-- awaitMany :: [Branch s a] -> Branch s [(a,s)]
-- awaitMany [] = return []
-- awaitMany (b:bs) = do
--   s0 <- get
--   (a,s) <- b
--   as <- awaitMany bs
--   put s0
--   return ((a,s):as)




prog :: Branch Int Int
prog = do
  copy 3
  increment

makeNChildren :: Int -> Branch Int Int
makeNChildren = undefined 
-- given a number n, make n children that is the 
--   product of the successor of the state at a 
--   given branch, and satisfies the predicate
makeNChildrenP :: Int -> (Int -> Bool) -> Branch Int (Proof Int)
makeNChildrenP = undefined



-- proofsBr :: Explain j => j -> Branch s (Proof j)

proofsBr :: (j -> [[j]]) -> j -> Branch s (Proof j)
proofsBr f j = 
  case f j of
    [] -> empty
    fj -> do
      -- j' <- save (concat fj)
      ps <- liftBr fj -- mapM (proofsBr f) j'
      case ps of
        [] -> return (Leaf j)
        _ -> do
          ps' <- mapM (proofsBr f) ps
          return (Proof j ps')
    -- (ps:pss) -> case ps of
    --   [] -> return (Leaf j)
    --   (_:pss) -> do
    --     ps' <- mapM (proofsBr f) ps
    --     return (Proof j ps')

pr1 :: Int -> [[Int]]
pr1 n | n `mod` 2 == 0 = [[n+1]]
      | n `mod` 9 == 0 = [[]]
      | otherwise = [[n+1], [n+2,n+2]]


prfs = map fst $ runBranch (proofsBr pr1 0) 0

nat 0 = "Z"
nat n = "S(" ++ nat (n-1) ++ ")"

inc :: Branch Int Int
inc = do
  s <- get
  put (s+1)
  return s

natsFromB :: Int -> Branch Int [Int]
natsFromB i = do
  return [i..]

syncCounts :: [Int] -> [Int]
syncCounts xs = concat $ transpose [[x..] | x <- xs]

syncCountsB :: [Int] -> Branch Int [Int]
syncCountsB xs = liftBr xs >>= \x -> return [x..]

runBInt :: Branch Int a -> [a]
runBInt = map fst . flip runBranch 0

nextTenFromB :: Int -> Branch Int Int
-- nextTenFromB n = lift [n..n+9]
nextTenFromB n = lift' [n..n+9]