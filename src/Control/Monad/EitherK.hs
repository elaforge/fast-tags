{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | A continuation-based error monad.
module Control.Monad.EitherK (
    EitherKT
    , runEitherKT
) where
import Control.Applicative
import Control.Monad.Error
import Control.Monad.State

newtype EitherKT e m a =
    EitherKT { runEitherKT :: forall r. (e -> m r) -> (a -> m r) -> m r }
    deriving (Functor)

instance Applicative (EitherKT e m) where
    pure x = EitherKT $ \_ sk -> sk x
    EitherKT f <*> EitherKT g = EitherKT $
        \ek sk -> f ek (\h -> g ek (\x -> sk $ h x))

instance Alternative (EitherKT String m) where
    empty = throwError "zero"
    EitherKT f <|> EitherKT f' = EitherKT $ \ek sk -> f (\_ -> f' ek sk) sk

instance Monad (EitherKT e m) where
    return = pure
    EitherKT f >>= m = EitherKT $ \ek sk ->
        f ek (\x -> runEitherKT (m x) ek sk)

instance MonadError e (EitherKT e m) where
    throwError e = EitherKT $ \ek _ -> ek e
    catchError (EitherKT f) handler = EitherKT $ \ek sk ->
        f (\e -> runEitherKT (handler e) ek sk) sk

instance (MonadState s m) => MonadState s (EitherKT e m) where
    get = EitherKT $ \_ sk -> get >>= sk
    put x = EitherKT $ \_ sk -> put x >>= sk
