module Control.Monad.Effector.State where

newtype State s a = State (s -> (a,s))

state :: (s -> (a,s)) -> State s a
state = State

runState :: State s a -> (s -> (a,s))
runState (State f) = f

instance Functor (State s) where
  fmap f (State g) = State $ (\(a,s) -> (f a,s)) . g

instance Applicative (State s) where
  pure a = State $ \s -> (a,s)
  (<*>) s1 s2 = do { f <- s1; a <- s2; return $ f a}

instance Monad (State s) where
  (>>=) (State g) f = State $ \s -> let (a,s')     = g s
                                        (State g') = f a in g' s'

