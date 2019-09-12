----------------------------------------------------------------------------
-- |
-- Module      :  FastTags.LensBlaze
-- Copyright   :  (c) Sergey Vinokurov 2019
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FastTags.LensBlaze
    ( Lens
    , Lens'
    , lens
    , view
    , over
    , set
    , int16L
    , int32L
    , intL
    ) where

import Control.Applicative

import Data.Bits
import Data.Functor.Identity
import Data.Int

type Lens' s a = Lens s s a a
type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

{-# INLINE lens #-}
lens :: (s -> a) -> (b -> s -> t) -> Lens s t a b
lens access write = \f s -> (\b -> write b s) <$> f (access s)

{-# INLINE view #-}
view :: Lens s t a b -> s -> a
view l = getConst . l Const

{-# INLINE set #-}
set :: Lens s t a b -> b -> s -> t
set l = over l . const

{-# INLINE over #-}
over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

{-# INLINE int16L #-}
int16L :: (Bits b, Integral b) => Int -> Lens' b Int16
int16L offset = intL offset 0xffff

{-# INLINE int32L #-}
int32L :: (Bits b, Integral b) => Int -> Lens' b Int32
int32L offset = intL offset 0xffffffff

{-# INLINE intL #-}
intL :: forall a b. (Integral a, Bits b, Integral b) => Int -> b -> Lens' b a
intL !offset !mask = \f x ->
    (\x' -> (fromIntegral x' `unsafeShiftL` offset) .|. (x .&. reverseMask)) <$>
    f (fromIntegral ((x `unsafeShiftR` offset) .&. mask :: b))
    where
    reverseMask :: b
    !reverseMask = complement $ mask `unsafeShiftL` offset

