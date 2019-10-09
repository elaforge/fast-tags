----------------------------------------------------------------------------
-- |
-- Module      :  FastTags.LensBlaze
-- Copyright   :  (c) Sergey Vinokurov 2019
--
-- Minimal implementation of lenses. The aim is to get lenses
-- that allow to work with parts of an Int as another Int.
--
-- E.g. to be able to treat upper 32 bits and lower 32 bits of an
-- integer as two different integers, read them independently and set
-- back.
--
-- This way values that have limited range will not occupy whole
-- pointer when stored in a struct, but rather will all be tightly
-- packed in a single integer "store" field that can be {-# UNPACK #-}'ed.
--
-- E.g. suppose we define 2d points that will be drawn on a computer
-- screen. We could start with
--
-- > data Point = Point
-- >    { getX :: {-# UNPACK #-} !Int
-- >    , getY :: {-# UNPACK #-} !Int
-- >    }
--
-- Now, this is not too bad but but observe that screens don't
-- typically require 64 bits of precision - 32 would be enough! If
-- only we could pack two Int32's into single Int64, that would occupy
-- twice as less space. But we don't want to have bit masks and shifts all
-- over the place in client code - that's error-prone. This is where this
-- module comes in. Instead of the above, we can do:
--
-- > newtype Point = Point { unPoint :: Int64 }
-- >
-- > pointL :: Lens Point Int64
-- > pointL = lens unPoint (\x _ -> Point x)
-- >
-- > xCoordL, yCoordL :: Lens Point Int32
-- > xCoordL = pointL . int32L 0
-- > yCoordL = pointL . int32L 32
--
-- Now, all the accesses to x and y coordinate have to go through lenses and
-- we lose record update syntax. But that's a manageable price and we get 2x
-- savings on memory traffic.
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

