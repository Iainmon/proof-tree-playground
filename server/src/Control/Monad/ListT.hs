{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.ListT where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Applicative ( Alternative(empty, (<|>)), liftA2 )
import Data.List (transpose)
import Control.Monad (MonadPlus(..))

import Data.Bifunctor
import Data.Maybe (fromMaybe)

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
  mzero = ListT $  return []
  mplus :: Monad m => ListT m a -> ListT m a -> ListT m a
  mplus m n = ListT $ do
    as <- runListT m
    bs <- runListT n
    return $ as +|+ bs

-- instance MonadPlus m => MonadPlus (ListT m) where
--   mzero :: MonadPlus m => ListT m a
--   mzero = ListT $ mzero -- return []
--   mplus :: MonadPlus m => ListT m a -> ListT m a -> ListT m a
--   mplus m n = ListT $ do
--     as <- runListT m
--     bs <- runListT n
--     return $ as +|+ bs

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


instance MonadState s m => MonadState s (ListT m) where
  get = lift get
  put = lift . put
  state = lift . state


newtype Vert a = Vert { runVert :: [a] }

instance Functor Vert where
  fmap f (Vert as) = Vert $ fmap f as




newtype ForkT g s a = ForkT { runForkT :: g -> s -> ([(a,s)],Maybe g) }

instance Functor (ForkT g s) where
  fmap :: (a -> b) -> ForkT g s a -> ForkT g s b
  fmap f frk = ForkT $ \g s -> first (fmap (first f)) $ runForkT frk g s

instance Applicative (ForkT g s) where
  pure :: a -> ForkT g s a
  pure a = ForkT $ \g s -> ([(a,s)],Just g)
  (<*>) :: ForkT g s (a -> b) -> ForkT g s a -> ForkT g s b
  frkF <*> frkA = do 
    f <- frkF
    a <- frkA
    pure $ f a


action :: (g -> s -> ([(a,s)],Maybe g)) -> g -> s -> ([(a,s)],g)
action f g s = second (fromMaybe g) $ f g s

forkAction :: ForkT g s a -> ForkT g s a
forkAction frk = ForkT $ \g s -> second Just $ action (runForkT frk) g s

runForkAction :: ForkT g s a -> g -> s -> ([(a,s)],g)
runForkAction frk = action $ runForkT frk


-- (>:>) :: ForkT g s a -> ForkT g s a -> ForkT g s a
-- frk1 >:> frk2 = ForkT $ \g s -> 
--   let ~(as,g') = runForkAction frk1 g s in 

-- composeForks :: Maybe g -> [(a,s)] -> (a -> ForkT g s b) -> [(b,s)]
-- composeForks mg as f = interleave $ map (\(a,s) -> runForkT (f a) mg s) as

composeForkActions :: [g -> ([(a,s)],Maybe g)] -> g -> ([[(a,s)]],Maybe g)
composeForkActions [] g = ([],Just g)
composeForkActions (f:fs) g = 
  let ~(as,g') = f g in
    let g'' = fromMaybe g g' in
      bimap (as:) (Just . fromMaybe g'') $ composeForkActions fs g''

instance Monad (ForkT g s) where
  (>>=) :: ForkT g s a -> (a -> ForkT g s b) -> ForkT g s b
  frk >>= f = ForkT $ \g s -> 
    let ~(as,gm) = runForkT frk g s in
      -- let g' = fromMaybe g gm in
        first interleave $ go as (fromMaybe g gm)
        -- let ~(bss,g'') = composeForkActions [flip (runForkT (f a)) s' | ~(a,s') <- as] g' in
        --   (interleave bss,g'')
        where go [~(a,s)] gst = bimap (:[]) (Just . fromMaybe gst) $ runForkT (f a) gst s 
              go ((~(a,s)):as) gst = 
                let ~(bs,gm) = runForkT (f a) gst s 
                  in let g' = if null gm then gst else fromMaybe gst gm 
                    in first (bs:) $ go as g'                 
              go [] gst = ([],Just gst)

instance Alternative (ForkT g s) where
  empty :: ForkT g s a
  empty = ForkT $ \g _ -> ([],Nothing)

  (<|>) :: ForkT g s a -> ForkT g s a -> ForkT g s a
  frk1 <|> frk2 = ForkT $ \g s -> 
    let ~(as1,gm1) = runForkT frk1 g s in
      let ~(as2,gm2) = runForkT frk2 (fromMaybe g gm1) s in
        (as1 +|+ as2, gm2 <|> gm1 <|> Just g)

globalGet :: ForkT g s g
globalGet = ForkT $ \g s -> ([(g,s)],Just g)

globalPut :: g -> ForkT g s ()
globalPut g = ForkT $ \_ s -> ([((),s)],Just g)

globalModify :: (g -> g) -> ForkT g s ()
globalModify f = ForkT $ \g s -> ([((),s)],Just $ f g)

globalGets :: (g -> a) -> ForkT g s a
globalGets f = ForkT $ \g s -> ([(f g,s)],Just g)

localGet :: ForkT g s s
localGet = ForkT $ \g s -> ([(s,s)],Just g)

localPut :: s -> ForkT g s ()
localPut s = ForkT $ \g _ -> ([((),s)],Just g)

localModify :: (s -> s) -> ForkT g s ()
localModify f = ForkT $ \g s -> ([((),f s)],Just g)

localGets :: (s -> a) -> ForkT g s a
localGets f = ForkT $ \g s -> ([(f s,s)],Just g)

save :: (s -> [a]) -> ForkT g s a
save f = ForkT $ \g s -> ([(a,s) | a <- f s],Just g)


forkEach :: [a] -> ForkT g s a
forkEach as = ForkT $ \g s -> ([(a,s) | a <- as],Just g)

each = forkEach

forkTake :: Int -> ForkT g s a -> ForkT g s a
forkTake n frk = ForkT $ \g s -> 
  let ~(as,g') = runForkT frk g s in
    (take n as,g')



forkN :: Int -> ForkT g s [Int]
forkN n = do
  guard $ n < 10
  forkEach [[n..(n + k)] | k <- [0..3]]


test' :: Int -> ForkT Int Int [Int]
test' n = do
  ns <- forkN (n + 1)
  localModify (+1)
  globalModify (+1)
  if length (tail ns) == 0
    then return ns
    else do
      ns' <- sequence $ map test' ns
      return $ n : concat ns'

test :: ForkT Int Int (Int,Int)
test = do
  a <- forkEach (map (10*) [1..])
  b <- forkEach (map (0-) [1..])
  pure $ (a, b)