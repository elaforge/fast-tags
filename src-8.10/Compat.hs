{-# LANGUAGE MagicHash #-}

-- Used with both GHC 8.10 and GHC 9.0.

module Compat
  ( int8ToInt#
  , int16ToInt#
  , int32ToInt#
  , wordToWord8#
  , wordToWord16#
  , wordToWord32#
  , word8ToWord#
  , word16ToWord#
  , word32ToWord#
  ) where

import GHC.Exts (Int#,Word#)

int8ToInt# :: Int# -> Int#
{-# inline int8ToInt# #-}
int8ToInt# x = x

int16ToInt# :: Int# -> Int#
{-# inline int16ToInt# #-}
int16ToInt# x = x

int32ToInt# :: Int# -> Int#
{-# inline int32ToInt# #-}
int32ToInt# x = x

wordToWord8# :: Word# -> Word#
{-# inline wordToWord8# #-}
wordToWord8#  x = x

wordToWord16# :: Word# -> Word#
{-# inline wordToWord16# #-}
wordToWord16# x = x

wordToWord32# :: Word# -> Word#
{-# inline wordToWord32# #-}
wordToWord32# x = x

word8ToWord# :: Word# -> Word#
{-# inline word8ToWord# #-}
word8ToWord# x = x

word16ToWord# :: Word# -> Word#
{-# inline word16ToWord# #-}
word16ToWord# x = x

word32ToWord# :: Word# -> Word#
{-# inline word32ToWord# #-}
word32ToWord# x = x
