{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FastTags.Token where

import Control.DeepSeq (NFData, rnf)
import Data.Text (Text)
import qualified Data.Text as Text


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

increaseLine :: Line -> Line
increaseLine (Line n) = Line $! n + 1

data SrcPos = SrcPos {
    posFile     :: !FilePath
    , posLine   :: {-# UNPACK #-} !Line
    -- | No need to keep prefix strict since most of the prefixes will not be
    -- used.
    , posPrefix :: Text
    } deriving (Eq, Ord)

instance NFData SrcPos where
    rnf (SrcPos x y z) = rnf x `seq` rnf y `seq` rnf z

instance Show SrcPos where
    show (SrcPos fn line prefix) =
        fn ++ ":" ++ show (unLine line) ++ prefix'
        where
        prefix' | Text.null prefix = ""
                | otherwise        = ":/" ++ Text.unpack prefix ++ "/"

data TokenVal =
    KWCase
    | KWClass
    | KWData
    | KWDefault
    | KWDeriving
    | KWDo
    | KWElse
    | KWFamily
    | KWForall
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
    | KWPattern
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
    | T !Text
    -- | Special token, not part of Haskell spec. Stores indentation.
    | Newline !Int
    -- | String contents is not tracked since it's irrelevant.
    | String
    -- | Actual character not tracked since it's irrelevant.
    | Character
    | QuasiquoterStart
    | QuasiquoterEnd
    | SpliceStart -- \$(
    | EOF
    deriving (Show, Eq, Ord)

type Token = Pos TokenVal
