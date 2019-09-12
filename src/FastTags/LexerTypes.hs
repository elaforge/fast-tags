----------------------------------------------------------------------------
-- |
-- Module      :  FastTags.LexerTypes
-- Copyright   :  (c) Sergey Vinokurov 2019
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

{-# OPTIONS_GHC -O2 #-}

module FastTags.LexerTypes
    ( Context(..)
    , AlexCode(..)
    , LitStyle(..)
    , LitMode(..)
    , isLiterateEnabled
    , isLiterateBirdInside
    , isLiterateLatexInside
    ) where

import Control.DeepSeq

data Context = CtxHaskell | CtxQuasiquoter
    deriving (Show, Eq, Ord)

-- | Abstract wrapper around alex automata states.
newtype AlexCode = AlexCode { unAlexCode :: Int }
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral, NFData)

data LitStyle = Bird | Latex
    deriving (Eq, Ord, Show, Enum, Bounded)

data LitMode a = LitInside !a | LitOutside | LitVanilla
    deriving (Eq, Ord, Show, Functor)

{-# INLINE isLiterateEnabled #-}
isLiterateEnabled :: LitMode a -> Bool
isLiterateEnabled = \case
    LitInside _ -> True
    LitOutside  -> True
    LitVanilla  -> False

{-# INLINE isLiterateBirdInside #-}
isLiterateBirdInside :: LitMode LitStyle -> Bool
isLiterateBirdInside = \case
    LitInside Bird  -> True
    LitInside Latex -> False
    LitOutside      -> True
    LitVanilla      -> False

{-# INLINE isLiterateLatexInside #-}
isLiterateLatexInside :: LitMode LitStyle -> Bool
isLiterateLatexInside = \case
    LitInside Bird  -> False
    LitInside Latex -> True
    LitOutside      -> False
    LitVanilla      -> False
