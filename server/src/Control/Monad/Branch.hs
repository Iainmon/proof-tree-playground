{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE InstanceSigs #-}
module Control.Monad.Branch where


-- import Control.Monad.Trans.State
-- import Control.Monad.Trans.Class

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Applicative ( Alternative(empty, (<|>)), liftA2 )
import Data.List (transpose)
import Data.Monoid (Ap(..))
import Data.Functor.Identity (Identity(..))

-- newtype State s a = State (s -> (a,s))
-- state :: Monad m => (s -> (a,s)) -> StateT s m a
-- state :: (Monad m, MonadState s m) => (s -> (a,s)) -> m a
-- runState :: State s a -> s -> (a,s)

-- data Branch s a = Branch (s -> [(a,s)])

interleave :: [[a]] -> [a]
interleave = concat . transpose

(+|+) :: [a] -> [a] -> [a]
xs +|+ ys = interleave [xs,ys]

(<*|*>) :: [a -> b] -> [a] -> [b]
fs <*|*> xs = interleave (map (flip map xs) fs)

newtype ListT m a = ListT { runListT :: m [a] }

instance Functor m => Functor (ListT m) where
  fmap :: Functor m => (a -> b) -> ListT m a -> ListT m b
  fmap f (ListT m) = ListT $ fmap (fmap f) m

instance Applicative m => Applicative (ListT m) where
  pure :: Applicative m => a -> ListT m a
  pure a = ListT $ pure [a]
  (<*>) :: Applicative m => ListT m (a -> b) -> ListT m a -> ListT m b
  f <*> v = ListT $ (<*|*>) <$> runListT f <*> runListT v

instance Monad m => Monad (ListT m) where
  (>>=) :: Monad m => ListT m a -> (a -> ListT m b) -> ListT m b
  ma >>= f = ListT $ do
    as <- runListT ma
    bs <- traverse (runListT . f) as
    return $ interleave bs

instance Applicative m => Alternative (ListT m) where
  empty :: Applicative m => ListT m a
  empty = ListT $ pure []
  
  (<|>) :: Applicative m => ListT m a -> ListT m a -> ListT m a
  m <|> n = ListT $ (+|+) <$> runListT m <*> runListT n

instance Monad m => MonadPlus (ListT m) where
  mzero :: Monad m => ListT m a
  mzero = ListT $ return []
  mplus :: Monad m => ListT m a -> ListT m a -> ListT m a
  mplus m n = ListT $ do
    as <- runListT m
    bs <- runListT n
    return $ as +|+ bs

instance Foldable f => Foldable (ListT f) where
  -- foldr :: Foldable f => (a -> b -> b) -> b -> ListT f a -> b
  foldr f z m = foldr (flip (foldr f)) z (runListT m)

instance Traversable f => Traversable (ListT f) where
  traverse f (ListT a) = ListT <$> traverse (traverse f) a


instance MonadTrans ListT where
  lift :: Monad m => m a -> ListT m a
  lift m = ListT $ fmap pure m

instance MonadIO m => MonadIO (ListT m) where
  liftIO :: MonadIO m => IO a -> ListT m a
  liftIO = lift . liftIO


-- instance Applicative m => Monoid (ListT m a) where
--   mempty :: Applicative m => ListT m a
--   mempty = ListT $ pure []

--   mappend :: Applicative m => ListT m a -> ListT m a -> ListT m a
--   mappend = (<|>)

-- newtype BranchT s m a = BranchT { runBranchT :: StateT s (ListT m) a }
-- newtype BranchT s m a = BranchT { runBranchT :: s -> m [(a,s)] }
--   deriving (Functor
--            ,Applicative
--            ,Monad
--            ,Alternative
--            ,MonadPlus
--           --  ,MonadTrans (BranchT s m)
--            ,MonadState s)
--     via (StateT s (ListT m))



-- toProofT = mapStateT toMulti

-- type Branch s a = StateT s [] a
-- type Branch s a = BranchT s Identity a
newtype BranchT s m a = BranchT { runBranchT :: s -> ListT m (a,s) }
  deriving (Functor
           ,Applicative
           ,Monad
           ,Alternative
           ,MonadPlus
           ,MonadState s)
           via (StateT s (ListT m))

instance MonadTrans (BranchT s) where
  lift :: Monad m => m a -> BranchT s m a
  lift m = BranchT $ \s -> ListT $ do
    a <- m
    return [(a,s)]

          --  ,MonadTrans (BranchT s)

-- type Branch s a = BranchT s Identity a
newtype Branch s a = Branch { runBranch :: s -> [(a,s)] }
  deriving (Functor
          ,Applicative
          ,Monad
          ,Alternative
          ,MonadPlus
          ,MonadState s)
          via (BranchT s Identity)

-- type Branch s a = BranchT s Identity a

-- pattern Branch :: (s -> [(a,s)]) -> Branch s a
-- pattern Branch f = StateT f

branch :: (s -> [(a,s)]) -> Branch s a
-- branch f = BranchT $ \s -> ListT $ Identity $ f s -- Branch
branch f = Branch $ \s -> f s -- Branch

run :: Branch s a -> (s -> [(a,s)])
run b s = runBranch b s
-- runBranch b s = runIdentity $ runListT $ runBranchT b s


-- instance Functor (Branch s) where
--   fmap f (Branch g) = branch $ map (\(a,s) -> (f a,s)) . g

-- instance Applicative (Branch s) where
--   pure a = branch $ \s -> pure (a,s)
--   (<*>) s1 s2 = do { f <- s1; a <- s2; return $ f a}

-- instance Monad (Branch s) where
--   -- (>>=) (Branch g) f = Branch $ \s -> concat [let (Branch g') = f a in g' s' | (a,s') <- g s]
--   e >>= f = branch $ \s -> run e s >>= (\(a,s') -> run (f a) s')

-- instance Alternative (Branch s) where
--   empty = Branch $ const [] 
--   (<|>) b1 b2 = branch $ \s -> run b1 s <|> run b2 s


save :: (s -> [a]) -> Branch s a
save f = branch $ \s -> [(a,s) | a <- f s]

-- branchState :: State s a -> Branch s a
-- branchState = branch . (return.) . runState

eff :: (s -> (a,s)) -> Branch s a
eff f = branch (pure . f)

modify :: (s -> s) -> Branch s ()
modify f = branch (\s -> [((),f s)])

-- get :: Branch s s
-- get = Branch (\s -> [(s,s)])

-- put :: s -> Branch s ()
-- put s = Branch (\_ -> [((),s)])

gets :: (s -> a) -> Branch s a
gets f = get >>= pure . f

-- class Transpose t where
--   transpose :: t [a] -> [t a]

each :: [a] -> Branch s a
each as = Branch $ \s -> map (,s) as

-- lift' ms = BranchT $ \s -> ListT $ Identity $ (map (,s)) ms
lift' :: [a] -> Branch s a
lift' x = Branch $ \s -> map (,s) x
-- lift' x = do empty; y <- lift (return x); z <- y; return z;
liftBr :: [a] -> Branch s a
-- liftBr m = Branch $ \s -> do
--   a <- m
--   return (a,s)
-- liftBr m = Branch $ \s -> m >>= \a -> return (a,s)
-- liftBr m = Branch $ \s -> concat $ map (\a -> return (a,s)) m
-- liftBr m = Branch $ \s -> map (\a -> (a,s)) m
-- liftBr m = branch $ \s -> [(a,s) | a <- m]
liftBr m = Branch $ \s -> do
  a <- m
  return (a,s)

liftBr' :: [a] -> Branch s a
liftBr' m = branch $ \s -> m >>= \a -> return (a,s)

reorder :: Branch s a -> Branch s a
reorder b = branch $ \s -> run b s


(>>==) :: Branch s a -> (a -> Branch s b) -> Branch s b
-- m >>== f = Branch $ \s -> do
--   ~(a,s') <- runStateT m s
--   runStateT (f a) s'
-- m >>== f = Branch $ \s -> runStateT m s >>= \(a,s') -> runStateT (f a) s'
m >>== f = branch $ \s -> concat [runBranch (f a) s' | ~(a,s') <- runBranch m s ]

(>>=|) :: Branch s a -> (a -> Branch s b) -> Branch s b
m >>=| f = Branch $ \s ->  interleave [runBranch (f a) s' | ~(a,s') <- runBranch m s ]


sequenceBr :: [Branch s a] -> Branch s [a]
-- sequenceBr bs = mapM id bs
-- sequenceBr as = foldr k (return []) as
--   where k a m = do 
--           a' <- a
--           as' <- m
--           return (a':as')
sequenceBr [] = return []
-- sequenceBr (b:bs) = do
--   a <- b
--   as <- sequenceBr bs
--   return (a:as)
-- sequenceBr (b:bs) = b >>= \a -> sequenceBr bs >>= \as -> return (a:as)
sequenceBr (b:bs) = branch $ \s -> [(a:as,s'') | (a,s') <- runBranch b s, (as,s'') <- runBranch (sequenceBr bs) s']

interSequence :: [Branch s a] -> Branch s [a]
interSequence [] = return []
interSequence (b:bs) = b >>=| \a -> interSequence bs >>=| \as -> return (a:as)
-- interSequence (m:ms) = Branch $ \s -> 
--   let ~(a,s') = runStateT m s


-- transposeLift :: [[a]] -> Branch s [a]
-- transposeLift as = lift (transpose as)
-- interLift [] = Branch $ \s -> [([],s)]
-- interLift ([]:as) = Branch $ \s -> 
--  = Branch $ \s ->


-- transposeB :: [Branch s a] -> Branch s [a]
-- transposeB ms = Branch $ \s ->
--   let xs = map (flip runStateT s) ms
--     in undefined
-- transposeB :: Branch s [a] -> Branch s [a]
-- transposeB m = Branch $ \s ->
--   let xs = runStateT m s in



mapMBr :: (a -> Branch s b) -> [a] -> Branch s [b]
mapMBr f as = foldr k (return []) as
  where k a m = do
          b <- f a
          bs <- m
          return (b:bs)

