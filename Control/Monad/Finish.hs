{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
-- Copyright © 2008 Josh Triplett and Jamey Sharp
-- See the file COPYING for license details.

-- |The 'FinishT' monad transformer is used to provide "early
-- return" from some monadic computation. It is typically
-- used at or near the top of the transformer stack.  FinishT is
-- implemented by skipping each monadic computation step after
-- a call to 'finish'; this will not be efficient unless the
-- compiler optimizes computation skips away.

module Control.Monad.Finish (
    FinishT(), finish, runFinishT
) where

import Control.Monad.Reader.Class
import Control.Monad.Trans

-- |The FinishT monad transformer type.
newtype FinishT r m a = FinishT { unFinishT :: m (Either r a) }

instance Monad m => Monad (FinishT r m) where
    return = FinishT . return . Right
    r >>= f = FinishT $ unFinishT r >>= either (return . Left) (unFinishT . f)

instance MonadTrans (FinishT r) where
    lift m = FinishT $ do m >>= return . Right

instance MonadIO m => MonadIO (FinishT r m) where
    liftIO = lift . liftIO

-- This does not meet the coverage condition, because MonadReader's functional
-- dependency requires (FinishT r m) -> v and the coverage condition thus
-- requires v to appear in (FinishT r m) which it doesn't.  To allow this
-- instance in spite of the coverage condition, we have to use
-- UndecidableInstances.
instance MonadReader v m => MonadReader v (FinishT r m) where
    ask = lift ask
    local f r = FinishT $ local f $ unFinishT r

-- |Return early from a computation.
finish :: Monad m => r -> FinishT r m b
finish = FinishT . return . Left

-- |Execute a monadic computation under FinishT.
runFinishT :: Monad m => FinishT r m r -> m r
runFinishT = (either return return =<<) . unFinishT
