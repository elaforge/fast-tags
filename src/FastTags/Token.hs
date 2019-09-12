{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module FastTags.Token where

import Control.DeepSeq (NFData, rnf)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

data Pos a = Pos {
    posOf   :: {-# UNPACK #-} !SrcPos
    , valOf :: !a
    } deriving (Eq, Ord)

instance (NFData a) => NFData (Pos a) where
    rnf (Pos x y) = rnf x `seq` rnf y

instance Show a => Show (Pos a) where
    show (Pos pos val) = show pos ++ ":" ++ show val

newtype Line = Line { unLine :: Int }
    deriving (Show, Eq, Ord, NFData, Num)

newtype Offset = Offset { unOffset :: Int }
    deriving (Show, Eq, Ord, NFData, Num)

increaseLine :: Line -> Line
increaseLine (Line n) = Line $! n + 1

data SrcPos = SrcPos {
    posFile     :: !FilePath
    , posLine   :: {-# UNPACK #-} !Line
    , posOffset :: {-# UNPACK #-} !Offset
    -- | No need to keep prefix strict since most of the prefixes will not be
    -- used.
    , posPrefix :: Text
    , posSuffix :: Text
    } deriving (Eq, Ord)

instance NFData SrcPos where
    rnf (SrcPos v w x y z) = rnf v `seq` rnf w `seq` rnf x `seq` rnf y `seq` rnf z

instance Show SrcPos where
    show (SrcPos fn line offset prefix suffix) =
        fn ++ ":" ++ show (unLine line) ++ ":" ++ show (unOffset offset) ++ prefix' ++ suffix'
        where
        prefix' = clean prefix
        suffix' = clean suffix
        clean s | Text.null s = ""
                | otherwise   = ":/" ++ Text.unpack s ++ "/"

forallTokenVal :: TokenVal
forallTokenVal = T "forall"

patternTokenVal :: TokenVal
patternTokenVal = T "pattern"

data TokenVal =
    KWCase
    | KWClass
    | KWData
    | KWDefault
    | KWDeriving
    | KWDo
    | KWElse
    | KWFamily
    | KWForeign
    | KWIf
    | KWImport
    | KWIn
    | KWInfix
    | KWInfixl
    | KWInfixr
    | KWInstance
    | KWLet
    | KWModule
    | KWNewtype
    | KWOf
    | KWThen
    | KWType
    | KWWhere
    | Arrow
    | At
    | Backtick
    | Comma
    | Dot
    | DoubleColon
    | Equals
    | ExclamationMark
    | Implies
    | LBrace
    | LBracket
    | LParen
    | Pipe
    | RBrace
    | RBracket
    | RParen
    | Tilde
    | Semicolon
    | T {-# UNPACK #-} !Text
    -- | Special token, not part of Haskell spec. Stores indentation.
    | Newline {-# UNPACK #-} !Int
    -- | String contents is not tracked since it's irrelevant.
    | String
    -- | Actual character not tracked since it's irrelevant.
    | Character
    -- | Actual value not tracked since it's irrelevant.
    | Number
    | QuasiquoterStart
    | QuasiquoterEnd
    | SpliceStart -- \$(, ends with RParen
    | ToplevelSplice -- e.g. \$foo
    | LambdaBackslash -- \
    | CppDefine {-# UNPACK #-} !Text
    | HSCEnum      -- #{enum...}
    | HSCDirective -- e.g. #define foo bar...
    | HSCDirectiveBraced
      -- ^ e.g. #{define foo...\nbar}, #{\ndefine foo...\nbar}, ends with RBrace
    | LBanana      -- Arrows: (|
    | RBanana      -- Arrows: |)
    | Error Text
    | DQuote -- '"' when not part of string in Alex or Happy
    | EOF
    deriving (Show, Eq, Ord, Generic)

instance NFData TokenVal

type Token = Pos TokenVal
