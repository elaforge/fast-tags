----------------------------------------------------------------------------
-- |
-- Module      :  FastTags.LexerTypes
-- Copyright   :  (c) Sergey Vinokurov 2019
--
-- Auxiliary types for lexer.
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

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

-- | Whether we're currently in Haskell context or within Template Haskell
-- quasiquoter and should discard as much as possible.
data Context = CtxHaskell | CtxQuasiquoter
    deriving (Show, Eq, Ord)

-- | Abstract wrapper around alex automata states.
newtype AlexCode = AlexCode { unAlexCode :: Int }
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral, NFData)

-- | Literate style we're currently in.
data LitStyle = Bird | Latex
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Literate mode of whole file. Either @LitVanilla@ all the time, which means
-- it's non-literate file or @LitOutside@ when looking at literate comments and
-- @LitInside mode@ when lexing Haskell source with specific literate @mode@.
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
