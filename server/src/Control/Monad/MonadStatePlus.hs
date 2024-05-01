{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.MonadStatePlus where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Class

class Monad m => MonadStatePlus g s m | m -> s, m -> g where
  globalPut :: g -> m ()
  globalPut g = globalState $ const ((),g)

  globalGet :: m g
  globalGet = globalState $ \g -> (g,g)

  globalState :: (g -> (a,g)) -> m a
  globalState f = do
    g <- globalGet
    let ~(a,g') = f g
    globalPut g'
    return a

  localPut :: s -> m ()
  localPut s = localState $ const ((),s)

  localGet :: m s
  localGet = localState $ \s -> (s,s)

  localState :: (s -> (a,s)) -> m a
  localState f = do
    s <- localGet
    let ~(a,s') = f s
    localPut s'
    return a

  {-# MINIMAL (globalState | globalPut, globalGet), (localState | localPut, localGet) #-}

-- instance (MonadTrans t, Monad m, MonadState s (t m)) => MonadStatePlus g s (StateT s (t m))
-- instance (MonadTrans t,MonadState s m) => MonadStatePlus g s (StateT s (t (StateT g m))) where


global :: (Monad m,MonadTrans mt,MonadStatePlus g s (mt m)) => StateT g m a -> mt m a
global s = do
  g <- globalGet
  (a,g') <- lift $ runStateT s g
  globalPut g'
  return a

local :: (Monad m,MonadTrans mt,MonadStatePlus g s (mt m)) => StateT s m a -> mt m a
local s = do
  g <- localGet
  (a,g') <- lift $ runStateT s g
  localPut g'
  return a
