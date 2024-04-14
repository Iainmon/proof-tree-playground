{-# LANGUAGE PatternSynonyms #-}
module Control.Monad.Effector where

import Control.Applicative ( Alternative(empty, (<|>)) )

import Data.Functor.Identity (Identity(..))

newtype Effector s m a = Effector (s -> m (a,s))

run :: Effector s m a -> s -> m (a,s)
run (Effector f) = f

effector :: (s -> m (a,s)) -> Effector s m a
effector = Effector

instance Functor m => Functor (Effector s m) where
  fmap f e = effector $ fmap (\(a,s) -> (f a,s)) . run e

instance Monad m => Applicative (Effector s m) where
  pure a = effector $ \s -> pure (a,s)
  e1 <*> e2 = do { f <- e1; a <- e2; return $ f a}

instance Monad m => Monad (Effector s m) where
  e >>= f = effector $ \s -> run e s >>= (\(a,s') -> run (f a) s')



eff :: Monad m => (s -> (a,s)) -> Effector s m a
eff f = effector (pure . f)

get :: Monad m => Effector s m s
get = eff $ \s -> (s,s) 

put :: Monad m => s -> Effector s m ()
put s = eff (const ((),s))

save :: Monad m => (s -> m a) -> Effector s m a
save f = effector $ \s -> f s >>= (\a -> pure (a,s))

modify :: Monad m => (s -> s) -> Effector s m ()
modify f = get >>= (put . f)

gets :: Monad m => (s -> a) -> Effector s m a
gets f = get >>= pure . f


-- type State s a = Effector s Identity a


-- type Branch s a = Effector s [] a

-- pattern Branch :: (s -> [(a,s)]) -> Branch s a
-- pattern Branch f = Effector f

-- type Parser a = Effector String [] a

-- save :: (s -> [a]) -> Branch s a
-- save f = branch $ \s -> [(a,s) | a <- f s]

-- branchState :: State s a -> Branch s a
-- branchState = branch . (return.) . runState

-- state :: (s -> (a,s)) -> State s a
-- state = State

-- -- runState :: State s a -> (s -> (a,s))
-- -- runState (State f) = f

-- branch :: (s -> [(a,s)]) -> Branch s a
-- branch = Branch