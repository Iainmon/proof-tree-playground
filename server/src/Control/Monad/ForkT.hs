{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}


module Control.Monad.ForkT where

import Control.Monad.Trans
import Control.Applicative ( Alternative(empty, (<|>)), liftA2 )
import Control.Monad (MonadPlus(..))
import Data.Functor.Identity (Identity(..))

import Control.Monad.ListT hiding (ForkT(..),each)
import Control.Monad.Branch (BranchT(..), Branch(..))
import Control.Monad.State

{-
ForkT g s m a = BranchT s (StateT g m) a
               = StateT s (ListT (StateT g m)) a
               = s -> ListT (StateT g m) (a,s)
               = s -> (StateT g m [(a,s)])
               = s -> g -> m ([(a,s)],g)
-}
newtype ForkT g s m a
  -- = ForkT { runForkT :: BranchT s (StateT g m) a }
  = ForkT { runForkT :: StateT s (ListT (StateT g m)) a }
    deriving (Functor
             , Applicative
            --  , Monad
             , Alternative
            --  , MonadPlus
             , MonadState s
            --  , MonadState s m => MonadState g
    ) via (StateT s (ListT (StateT g m)))
            --  ) via (BranchT s (StateT g m))

instance Monad m => Monad (ForkT g s m) where
  (>>=) :: Monad m => ForkT g s m a -> (a -> ForkT g s m b) -> ForkT g s m b
  m >>= f = ForkT $ StateT $ (runStateT (runForkT m) >=> (\ (a, s') -> runStateT (runForkT (f a)) s'))

-- instance Monad m => MonadPlus (ForkT g s m) where
--   mzero :: Monad m => ForkT g s m a
--   mzero = ForkT $ StateT $ \s -> ListT $ StateT $ \g -> return ([],g)

--   mplus :: Monad m => ForkT g s m a -> ForkT g s m a -> ForkT g s m a
--   mplus m n = ForkT $ StateT $ \s -> ListT $ StateT $ \g -> do
--     (as,g') <- runStateT (runListT (runStateT (runForkT m) s)) g
--     (bs,g'') <- runStateT (runListT (runStateT (runForkT n) s)) g'
--     return (as ++ bs,g'')

  -- ForkT m >>= f = ForkT $ do
  --   a <- m
  --   runForkT $ f a
  -- ForkT m >>= f = ForkT $ StateT $ \s -> do
  --   (a,s') <- runStateT m s
  --   runStateT (runForkT $ f a) s'
  -- (ForkT (StateT sf)) >>= f = ForkT $ StateT $ \s ->
  --   let ListT (StateT gf) = sf s in ListT $ StateT $ \g -> 
  --     let m = gf g in m >>= \(lcs,g') -> 
  --       ListT lcs


toRep :: ForkT g s m a -> StateT s (ListT (StateT g m)) a
toRep = undefined

fromRep :: StateT s (ListT (StateT g m)) a -> ForkT g s m a
fromRep = undefined

forkT :: Monad m => (s -> g -> m ([(a,s)],g)) -> ForkT g s m a
forkT f = ForkT $ StateT $ \s -> ListT $ StateT $ \g -> f s g

unforkT :: Monad m => ForkT g s m a -> s -> g -> m ([(a,s)],g)
unforkT f s g = runStateT (runListT (runStateT (runForkT f) s)) g


instance MonadTrans (ForkT g s) where
  lift :: Monad m => m a -> ForkT g s m a
  lift m = ForkT $ StateT $ \s -> ListT $ StateT $ \g -> m >>= \a -> return ([(a,s)],g)

  -- lift m = ForkT $ lift $ lift $ lift m


local :: Monad m => StateT s m a -> ForkT g s m a
local s = ForkT $ StateT $ \s' -> lift $ lift $ runStateT s s'

global :: Monad m => StateT g m a -> ForkT g s m a
global s = ForkT $ lift $ lift s

getLocal :: Monad m => ForkT g s m s
getLocal = local get

getGlobal :: Monad m => ForkT g s m g
getGlobal = global get

putLocal :: Monad m => s -> ForkT g s m ()
putLocal s = local $ put s

putGlobal :: Monad m => g -> ForkT g s m ()
putGlobal g = global $ put g

type Fork g s a = ForkT g s Identity a

fork :: (g -> s -> ([(a,s)],g)) -> Fork g s a
fork f = forkT $ \s g -> return $ f g s


each :: [a] -> Fork g s a
each as = forkT $ \s g -> return (map (,s) as,g)



runFork :: Fork g s a -> g -> s -> ([(a,s)],g)
runFork f s g = runIdentity $ unforkT f g s

kill :: Fork g s a
kill = fork $ \g s -> ([],g)

terminate :: ForkT g s Maybe a
terminate = ForkT $ StateT $ \s -> ListT $ StateT $ const Nothing

-- local :: StateT s m a -> ForkT g s m a

-- _ :: Monad m => Fork g s a -> ForkT g s m a


incF :: Monad m => StateT Int m ()
incF = do
  n <- get
  put $ n + 1

killIfEven :: Int -> Fork g s Int
killIfEven n = if even n then kill else return n

termIfEven :: Int -> ForkT g s Maybe Int
termIfEven n = if even n then terminate else return n


prog :: Fork g Int Int
prog = do
  n <- each [1,2,3]
  putLocal n
  local incF
  n' <- getLocal
  return n'
  -- killIfEven n'

runs :: Int -> Fork g Int Int
runs n = do
  each [n+1..n+10]

runs' :: Int -> Fork g Int [Int]
runs' n = do
  n' <- runs n
  ns <- sequence $ map runs' (if even n' then [] else [n'])
  return $ n' : concat ns

takeFork :: Fork g s a -> Int -> g -> s -> [(a,s)]
takeFork f n g s = take n $ fst $ runFork f g s
