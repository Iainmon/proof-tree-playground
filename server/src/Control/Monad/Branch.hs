module Control.Monad.Branch where


import Control.Applicative ( Alternative(empty, (<|>)) )


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


data Branch s a = Branch (s -> [(a,s)])

branch :: (s -> [(a,s)]) -> Branch s a
branch = Branch

run :: Branch s a -> (s -> [(a,s)])
run (Branch f) = f

instance Functor (Branch s) where
  fmap f (Branch g) = branch $ map (\(a,s) -> (f a,s)) . g

instance Applicative (Branch s) where
  pure a = branch $ \s -> pure (a,s)
  (<*>) s1 s2 = do { f <- s1; a <- s2; return $ f a}

instance Monad (Branch s) where
  -- (>>=) (Branch g) f = Branch $ \s -> concat [let (Branch g') = f a in g' s' | (a,s') <- g s]
  e >>= f = branch $ \s -> run e s >>= (\(a,s') -> run (f a) s')

instance Alternative (Branch s) where
  empty = Branch $ const [] 
  (<|>) b1 b2 = branch $ \s -> run b1 s <|> run b2 s


save :: (s -> [a]) -> Branch s a
save f = branch $ \s -> [(a,s) | a <- f s]

branchState :: State s a -> Branch s a
branchState = branch . (return.) . runState

eff :: (s -> (a,s)) -> Branch s a
eff f = Branch (pure . f)

modify :: (s -> s) -> Branch s ()
modify f = Branch (\s -> [((),f s)])

get :: Branch s s
get = Branch (\s -> [(s,s)])

put :: s -> Branch s ()
put s = Branch (\_ -> [((),s)])

gets :: (s -> a) -> Branch s a
gets f = get >>= pure . f
