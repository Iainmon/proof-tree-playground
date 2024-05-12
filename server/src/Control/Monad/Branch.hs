{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Control.Monad (MonadPlus(..))
-- newtype State s a = State (s -> (a,s))
-- state :: Monad m => (s -> (a,s)) -> StateT s m a
-- state :: (Monad m, MonadState s m) => (s -> (a,s)) -> m a
-- runState :: State s a -> s -> (a,s)

-- data Branch s a = Branch (s -> [(a,s)])

import Control.Monad.ListT hiding (each)
import Control.Monad.MonadStatePlus



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


-- class Monad m => MonadBranch 


-- type Branch s a = BranchT s Identity a
newtype Branch s a = Branch { runBranch :: s -> [(a,s)] }
  deriving (Functor
          ,Applicative
          ,Monad
          ,Alternative
          ,MonadPlus
          ,MonadState s)
          via (BranchT s Identity)

-- newtype BrancherT g s m a = BrancherT { runBrancherT :: s -> g -> m ([(a,s)],g) }
-- StateT g (BranchT s m) a = g -> BranchT s m (a,g)
--                          = g -> s -> ListT m ((a,s),g)
--                          = g -> s -> m [((a,s),g)]
-- g -> s -> m ([(a,s)],g) = 
newtype BrancherT g s m a = BrancherT { runBrancherT :: StateT s (ListT (StateT g m)) a }
  deriving (Functor
          ,Applicative
          ,Monad
          ,Alternative
          ,MonadPlus
          -- ,MonadState s
          )
          -- via (StateT s (ListT (StateT g m)))
          via (BranchT s (StateT g m))


instance Monad m => MonadState s (BrancherT g s m) where
  get = BrancherT $ do
    get
  put s = BrancherT $ do
    put s


instance MonadTrans (BrancherT g s) where
  lift :: Monad m => m a -> BrancherT g s m a
  lift m = BrancherT $ StateT $ \s -> ListT $ StateT $ \g -> do
    a <- m
    return (pure (a,s),g)
  -- lift m = BrancherT $ do
  --   a <- lift $ lift $ lift m
  --   return a
  -- lift m = BrancherT $ \s -> ListT $ StateT $ \g -> do
  --   a <- m
  --   return (pure (a,s),g)

instance Monad m => MonadStatePlus g s (BrancherT g s m) where
  globalGet = BrancherT $ StateT $ \s -> ListT $ do
    g <- get
    return $ pure (g,s)
  globalPut g = BrancherT $ StateT $ \s -> ListT $ do
    put g
    return $ pure ((),s)
  -- globalState :: Monad m => (g -> (a, g)) -> BrancherT g s m a
  -- globalState f = BrancherT $ \s -> do
  --   g <- lift $ lift $ get
  --   let ~(a,g') = f g
  --   lift $ lift $ put g'
  --   lift $ lift $ return (a,s)
    -- ListT $ StateT $ \g -> return ([(fst (f g),s)],snd (f g))
  localGet = BrancherT $ StateT $ \s -> ListT $ StateT $ \g -> return $ (pure (s,s),g)
  localPut s = BrancherT $ StateT $ \_ -> ListT $ StateT $ \g -> return $ (pure ((),s),g)
newtype Brancher g s a = Brancher { runBrancher :: s -> g -> ([(a,s)],g) }
  deriving (Functor
          ,Applicative
          ,Monad
          ,Alternative
          ,MonadPlus
          ,MonadState s
          ,MonadStatePlus g s
          -- ,MonadBranch s
          )
          via (BrancherT g s Identity)

-- toBrancherT :: Brancher g s a -> BrancherT g s Identity a
-- toBrancherT (Brancher f) = BrancherT $ \s -> ListT $ StateT $ \g -> return $ f s g

-- instance Monad (Brancher g s) where
--   (>>=) :: Brancher g s a -> (a -> Brancher g s b) -> Brancher g s b
--   m >>= f = 


-- putGlobal :: Monad m => g -> BrancherT g s m ()
-- putLocal :: Monad m => s -> BrancherT g s m ()

-- class MonadBranch s m => MonadBrancher g s m | m -> s, m -> g where
--   branches :: Monad m' => m a -> BranchT s m' a
--   global :: Monad m' => m a -> StateT g m' a
--   brancher :: (s -> g -> ([(a,s)],g)) -> m a
-- class MonadState s m => MonadBrancher s m | m -> s where
  

-- class (Monad m, MonadState s m) => MonadBranch s m | m -> s where
--   each :: [a] -> m a
  -- branch :: (s -> [(a,s)]) -> m a
-- class (Monad m,MonadBranch s m) => MonadBrancher g s m | m -> s, m -> g where



class MonadState s m => MonadBranch s m | m -> s where
  each :: [a] -> m a
  each as = branch $ \s -> map (,s) as
  branch :: (s -> [(a,s)]) -> m a
  {-# MINIMAL branch #-}


instance Monad m => MonadBranch s (BranchT s m) where
  each :: Monad m => [a] -> BranchT s m a
  each as = BranchT $ \s -> ListT $ return $ map (,s) as
  branch :: (s -> [(a,s)]) -> BranchT s m a
  branch f = BranchT $ \s -> ListT $ return $ f s

instance MonadBranch s (Branch s) where
  each :: [a] -> Branch s a
  each as = Branch $ \s -> map (,s) as
  branch :: (s -> [(a,s)]) -> Branch s a
  branch f = Branch $ \s -> f s

instance MonadBranch s m => MonadBranch s (StateT s m) where
  each :: Monad m => [a] -> StateT s m a
  each as = do
    each as
  branch :: (s -> [(a,s)]) -> StateT s m a
  branch f = lift (branch f)

instance MonadBranch s m => MonadBranch s (ListT m) where
  each :: Monad m => [a] -> ListT m a
  each as = do
    each as
  branch :: (s -> [(a,s)]) -> ListT m a
  branch f = lift (branch f)

-- instance MonadState s m => MonadBranch s m where
--   branch f = do
--     s <- get
--     ~(a,s') <- each (f s)
--     put s'
--     return a

-- instance MonadBranch m

-- test :: MonadBranch Int m => m (Int,Int)
-- test = do
--   a <- each [-1,1]
--   b <- each [4,5,6]
--   put (a*b)
--   return (a,b)
-- test :: MonadBranch Int m => m [(Int,Int)]
-- test = do
--   each [1,2,3] >>= put
--   sequence $ map (\a -> do {b <- each [4,5,6]; return (a,a*b)}) [-1,1]
  
-- t1 = runBranch test 1
-- t2 = runIdentity $ runListT $ runBranchT test 1
-- t3 = runBrancher test 1 1
-- t4 = runIdentity $ runBrancherT test 1 1

class (Monad m, MonadState s m, MonadStatePlus g s m) => MonadBrancher g s m | m -> s, m -> g where
  brancher :: (s -> g -> ([(a,s)],g)) -> m a



instance Monad m => MonadBrancher g s (BrancherT g s m) where
  brancher :: Monad m => (s -> g -> ([(a, s)], g)) -> BrancherT g s m a
  brancher f = BrancherT $ StateT$ \s -> ListT $ StateT $ \g -> return $ f s g

instance MonadBrancher g s (Brancher g s) where
  brancher :: (s -> g -> ([(a, s)], g)) -> Brancher g s a
  brancher f = Brancher $ \s g -> f s g

instance Monad m => MonadBranch s (BrancherT g s m) where
  -- each :: Monad m => [a] -> BrancherT g s m a
  each as = BrancherT $ StateT $ \s -> ListT $ StateT $ \g -> return $ (,g) $ map (,s) as
  -- each as = do
  --   s <- get
  --   msum (map (\a -> do {put s; return a}) as)

  -- each as = BrancherT $ do
  --   lift $ ListT (return as)
  branch :: (s -> [(a,s)]) -> BrancherT g s m a
  branch f = BrancherT $ StateT $ \s -> ListT $ return $ f s

-- instance MonadBranch s (Brancher g s) where
--   each :: [a] -> Brancher g s a
--   each as = Brancher $ \s -> (map (,s) as,)
--   branch :: (s -> [(a,s)]) -> Brancher g s a
--   branch f = Brancher $ \s -> (f s,)
 
interSeq :: Monad m => [BrancherT g s m a] -> BrancherT g s m [a]
interSeq [] = return []
interSeq (m:ms) = do
  a <- m
  as <- interSeq ms
  return (a:as)


-- instance MonadBrancher g s m => MonadBranch s m where
--   each as = brancher $ \s g -> (map (,s) as,g)

-- newtype BranchT' s1 s2 m a = BranchT' { runBranchT' :: s2 -> s1 -> m ([(a,s2)],s1)}

-- type Branch s a = BranchT s Identity a

-- pattern Branch :: (s -> [(a,s)]) -> Branch s a
-- pattern Branch f = StateT f

-- branch :: (s -> [(a,s)]) -> Branch s a
-- -- branch f = BranchT $ \s -> ListT $ Identity $ f s -- Branch
-- branch f = Branch $ \s -> f s -- Branch

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

-- modify :: (s -> s) -> Branch s ()
-- modify f = branch (\s -> [((),f s)])

-- global :: (s -> s) -> Branch s ()
-- global f = branch (\s -> [((),f s)])

terminate :: Branch s a
-- terminate = empty
terminate = branch (const [])


-- get :: Branch s s
-- get = Branch (\s -> [(s,s)])

-- put :: s -> Branch s ()
-- put s = Branch (\_ -> [((),s)])

-- gets :: (s -> a) -> Branch s a
-- gets f = get >>= pure . f

-- class Transpose t where
--   transpose :: t [a] -> [t a]

-- each :: [a] -> Branch s a
-- each as = Branch $ \s -> map (,s) as

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

