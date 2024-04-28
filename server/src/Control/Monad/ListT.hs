module Control.Monad.ListT where

import Control.Monad.Trans
import Control.Applicative ( Alternative(empty, (<|>)), liftA2 )
import Data.List (transpose)
import Control.Monad (MonadPlus(..))


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
