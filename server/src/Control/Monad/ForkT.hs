{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}


module Control.Monad.ForkT where

import Control.Monad.Trans
import Control.Applicative ( Alternative(empty, (<|>)), liftA2 )
import Control.Monad (MonadPlus(..))
import Data.Functor.Identity (Identity(..))

import Control.Monad.ListT
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
             , Monad
             , Alternative
             , MonadPlus
             , MonadState s
            --  , MonadState s m => MonadState g
    ) via (StateT s (ListT (StateT g m)))
             -- ) via (BranchT s (StateT g m))

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

terminate :: Fork g s a
terminate = ForkT $ StateT $ \s -> ListT $ StateT $ \g -> return ([],g)

-- local :: StateT s m a -> ForkT g s m a

-- _ :: Monad m => Fork g s a -> ForkT g s m a


incF :: State Int ()
incF = modify (+1)

killIfEven :: Int -> Fork g s Int
killIfEven n = if even n then kill else return n

termIfEven :: Int -> Fork g s Int
termIfEven n = if even n then terminate else return n



