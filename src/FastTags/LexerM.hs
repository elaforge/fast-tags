----------------------------------------------------------------------------
-- |
-- Module      :  FastTags.LexerM
-- Copyright   :  (c) Sergey Vinokurov 2019
--
-- All the types and functions needed to make lexer run:
-- - 'AlexInput' - primary workhorse, an optimized representation of input
--   stream as a pointer to utf8 bytes and our position within it.
-- - Lexer monad 'AlexM' - a monad (self-explanatory) with state that describes
--   current lexing context.
-- - 'AlexState' - state of the lexing monad, maintains current Alex code,
--   comment depth, quasiquoter depth, indentation size, whether we're in
--   a literate mode (and in which one) or vanilla mode and whether there
--   are any TH quasiquotes present till the end of file.
--
-- All the functions are to do with
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UnboxedTuples              #-}

module FastTags.LexerM
    ( AlexState(..)
    , mkAlexState
    , alexEnterBirdLiterateEnv
    , alexEnterLiterateLatexEnv
    , alexExitLiterateEnv
    , pushContext
    , modifyCommentDepth
    , modifyQuasiquoterDepth
    , modifyPreprocessorDepth
    , addIndentationSize
    , checkQuasiQuoteEndPresent

    , AlexM
    , runAlexM
    , alexSetInput
    , alexSetNextCode

    , AlexInput(..)
    , aiLineL
    , takeText
    , countInputSpace
    , extractDefineOrLetName
    , dropUntilNL
    , dropUntilUnescapedNL
    , dropUntilNLOr
    , dropUntilNLOrEither
    , unsafeTextHeadAscii
    , unsafeTextHeadOfTailAscii
    , unsafeTextHead
    , utf8BS

    , asCodeL
    , asCommentDepthL
    , asQuasiquoterDepthL
    , asIndentationSizeL
    , asPreprocessorDepthL
    , asLiterateLocL
    , asHaveQQEndL

      -- * Alex interface
    , alexInputPrevChar
    , alexGetByte
    ) where

import Control.Applicative as A
import Control.DeepSeq
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import Data.Char
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Void (Void, vacuous)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.Base
import GHC.Ptr
import GHC.Word
import Text.Printf

import FastTags.LensBlaze
import FastTags.LexerTypes
import FastTags.Token

data AlexState = AlexState
    { asInput        :: {-# UNPACK #-} !AlexInput
    , asIntStore     :: {-# UNPACK #-} !Word64
        -- ^ Integer field that stores all the other useful fields for lexing.
    , asContextStack :: [Context]
    } deriving (Show, Eq, Ord)

{-# INLINE asIntStoreL #-}
asIntStoreL :: Lens' AlexState Word64
asIntStoreL = lens asIntStore (\b s -> s { asIntStore = b })

{-# INLINE maybeBoolToInt #-}
-- | Encode 'Maybe Bool' as bit mask to store it within integer store.
maybeBoolToInt :: Maybe Bool -> Int
maybeBoolToInt = \case
    Nothing    -> 0
    Just False -> 1
    Just True  -> 2

{-# INLINE intToMaybeBool #-}
-- | Decofe 'Maybe Bool' from bit mask stored within integer store.
intToMaybeBool :: Int -> Maybe Bool
intToMaybeBool = \case
    0 -> Nothing
    1 -> Just False
    2 -> Just True
    x -> error $ "Invalid integer representation of 'Maybe Bool': " ++ show x

{-# INLINE asCodeL              #-}
{-# INLINE asCommentDepthL      #-}
{-# INLINE asQuasiquoterDepthL  #-}
{-# INLINE asIndentationSizeL   #-}
{-# INLINE asPreprocessorDepthL #-}
{-# INLINE asLiterateLocL       #-}
{-# INLINE asHaveQQEndL         #-}
-- | Current Alex state the lexer is in. E.g. comments, string, TH quasiquoter
-- or vanilla toplevel mode.
asCodeL :: Lens' AlexState AlexCode
asCommentDepthL, asQuasiquoterDepthL, asIndentationSizeL :: Lens' AlexState Int16
-- | How many directives deep are we.
asPreprocessorDepthL :: Lens' AlexState Int16
-- | Whether we're in bird-style or latex-style literate environment
asLiterateLocL :: Lens' AlexState (LitMode LitStyle)
asHaveQQEndL   :: Lens' AlexState (Maybe Bool)
asCodeL              = asIntStoreL . intL 0  0x000f
asCommentDepthL      = asIntStoreL . intL 4  0x03ff
asQuasiquoterDepthL  = asIntStoreL . intL 14 0x03ff
asIndentationSizeL   = asIntStoreL . int16L  24
asPreprocessorDepthL = asIntStoreL . int16L  40
asLiterateLocL       = \f -> asIntStoreL (intL 56 0x0003 (fmap litLocToInt    . f . intToLitLoc))
asHaveQQEndL         = \f -> asIntStoreL (intL 58 0x0003 (fmap maybeBoolToInt . f . intToMaybeBool))

{-# INLINE litLocToInt #-}
litLocToInt :: LitMode LitStyle -> Int
litLocToInt = \case
    LitVanilla      -> 0
    LitOutside      -> 1
    LitInside Bird  -> 2
    LitInside Latex -> 3

{-# INLINE intToLitLoc #-}
intToLitLoc :: Int -> LitMode LitStyle
intToLitLoc = \case
    0 -> LitVanilla
    1 -> LitOutside
    2 -> LitInside Bird
    3 -> LitInside Latex
    x -> error $ "Invalid literate location representation: " ++ show x

mkAlexState :: LitMode Void -> AlexCode -> AlexInput -> AlexState
mkAlexState litLoc startCode input =
    set asCodeL startCode $
    set asLiterateLocL (vacuous litLoc) AlexState
        { asInput        = input
        , asIntStore     = 0
        , asContextStack = []
        }

{-# INLINE alexEnterBirdLiterateEnv #-}
alexEnterBirdLiterateEnv :: MonadState AlexState m => m ()
alexEnterBirdLiterateEnv =
    modify $ set asLiterateLocL (LitInside Bird)

{-# INLINE alexEnterLiterateLatexEnv #-}
alexEnterLiterateLatexEnv :: MonadState AlexState m => m ()
alexEnterLiterateLatexEnv =
    modify $ set asLiterateLocL (LitInside Latex)

{-# INLINE alexExitLiterateEnv #-}
alexExitLiterateEnv :: MonadState AlexState m => m ()
alexExitLiterateEnv =
    modify $ set asLiterateLocL LitOutside

{-# INLINE pushContext #-}
pushContext :: MonadState AlexState m => Context -> m ()
pushContext ctx =
    modify (\s -> s { asContextStack = ctx : asContextStack s })

{-# INLINE modifyCommentDepth #-}
modifyCommentDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16
modifyCommentDepth f = do
    depth <- gets (view asCommentDepthL)
    let !depth' = f depth
    modify $ \s -> set asCommentDepthL depth' s
    return depth'

{-# INLINE modifyQuasiquoterDepth #-}
modifyQuasiquoterDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16
modifyQuasiquoterDepth f = do
    depth <- gets (view asQuasiquoterDepthL)
    let !depth' = f depth
    modify $ \s -> set asQuasiquoterDepthL depth' s
    return depth'

{-# INLINE modifyPreprocessorDepth #-}
modifyPreprocessorDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16
modifyPreprocessorDepth f = do
    depth <- gets (view asPreprocessorDepthL)
    let !depth' = f depth
    modify $ \s -> set asPreprocessorDepthL depth' s
    return depth'

{-# INLINE alexSetInput #-}
alexSetInput :: MonadState AlexState m => AlexInput -> m ()
alexSetInput input = modify $ \s -> s { asInput = input }

{-# INLINE alexSetNextCode #-}
alexSetNextCode :: MonadState AlexState m => AlexCode -> m ()
alexSetNextCode code = modify $ set asCodeL code

{-# INLINE addIndentationSize #-}
addIndentationSize :: MonadState AlexState m => Int16 -> m ()
addIndentationSize x =
  modify (over asIndentationSizeL (+ x))

data QQEndsState = QQEndsState
    { qqessPresent  :: !Int#
    , qqessPrevChar :: !Char#
    }

checkQuasiQuoteEndPresent :: Ptr Word8 -> Bool
checkQuasiQuoteEndPresent
    = (\x -> isTrue# (qqessPresent x))
    . utf8Foldl' combine (QQEndsState 0# '\n'#)
    where
    combine :: QQEndsState -> Char# -> QQEndsState
    combine QQEndsState{qqessPresent, qqessPrevChar} c# = QQEndsState
        { qqessPresent      =
          qqessPresent `orI#`
          case (# qqessPrevChar, c# #) of
              (# '|'#, ']'# #) -> 1#
              (# _,    '⟧'# #) -> 1#
              _                -> 0#
        , qqessPrevChar = c#
        }

type AlexM = WriterT [(AlexInput, TokenVal)] (State AlexState)

{-# INLINE runAlexM #-}
runAlexM
  :: FilePath
  -> Bool
  -> LitMode Void
  -> AlexCode
  -> C8.ByteString
  -> AlexM a
  -> (a, [Token])
runAlexM filepath trackPrefixesAndOffsets litLoc startCode input action =
    withAlexInput input $ \input' inputSize ->
        let (a, xs) = evalState (runWriterT action)
                    $ mkAlexState litLoc startCode input'
        in if trackPrefixesAndOffsets
        then
            let !ptr  = aiPtr input' `plusPtr` 1 -- Drop first newline
                !size = inputSize - 1
                !idx  = positionsIndex ptr size
                res   =
                    map (\(x, y) -> Pos (mkSrcPos filepath idx ptr x) y) xs
            in res `deepseq` (a, res)
        else
            (a, map (\(x, y) -> Pos (mkSrcPosNoPrefix filepath x) y) xs)

mkSrcPosNoPrefix :: FilePath -> AlexInput -> SrcPos
mkSrcPosNoPrefix filename input =
    SrcPos { posFile   = filename
           , posLine   = view aiLineL input
           , posOffset = Offset 0
           , posPrefix = mempty
           , posSuffix = mempty
           }

mkSrcPos :: FilePath -> U.Vector Int -> Ptr Word8 -> AlexInput -> SrcPos
mkSrcPos filename bytesToCharsMap start (input@AlexInput {aiPtr}) =
    SrcPos { posFile = filename
           , posLine = view aiLineL input
           , posOffset
           , posPrefix
           , posSuffix
           }
    where
    lineLen   = view aiLineLengthL input
    posPrefix = TE.decodeUtf8 $ bytesToUtf8BS lineLen $ plusPtr aiPtr $ negate lineLen
    posSuffix = TE.decodeUtf8 $ regionToUtf8BS aiPtr $ dropUntilNL# aiPtr
    posOffset = Offset $ U.unsafeIndex bytesToCharsMap $ minusPtr aiPtr start


-- Vector mapping absolute offsets off a pointer into how many utf8 characters
-- were encoded since the pointer start.
positionsIndex :: Ptr Word8 -> Int -> U.Vector Int
positionsIndex (Ptr start#) len =
    U.create $ do
        (vec :: UM.MVector s Int) <- UM.new len
        let assignAfter :: Int -> Int -> Int -> ST s ()
            assignAfter start n item = go' n start
                where
                go' :: Int -> Int -> ST s ()
                go' 0  !i = UM.unsafeWrite vec i item
                go' !k !i = UM.unsafeWrite vec i item *> go' (k - 1) (i + 1)
            go :: Int# -> Int -> ST s ()
            go bytes# !nChars =
                case utf8SizeChar# (start# `plusAddr#` bytes#) of
                    0#      -> pure ()
                    nBytes# -> do
                        assignAfter (I# bytes#) (I# nBytes#) nChars
                        go (bytes# +# nBytes#) $ nChars + 1
        go 0# 0
        A.pure vec


data AlexInput = AlexInput
    { aiPtr      :: {-# UNPACK #-} !(Ptr Word8)
    , aiIntStore :: {-# UNPACK #-} !Word64
        -- ^ Integer field that stores all the other useful fields for lexing.
    } deriving (Eq, Ord)

instance Show AlexInput where
    show AlexInput{aiPtr, aiIntStore} =
        printf "AlexInput 0x%08x 0x%08x" ptr aiIntStore
        where
        ptr :: Word
        ptr = fromIntegral $ ptrToWordPtr aiPtr

{-# INLINE aiIntStoreL #-}
aiIntStoreL :: Lens' AlexInput Word64
aiIntStoreL = lens aiIntStore (\b s -> s { aiIntStore = b })

lineInt32L :: Lens' Int32 Line
lineInt32L = lens (Line . fromIntegral) (\(Line x) _ -> fromIntegral x)

int2Int32L :: Lens' Int32 Int
int2Int32L = lens fromIntegral (\x _ -> fromIntegral x)

{-# INLINE aiLineL       #-}
{-# INLINE aiLineLengthL #-}
-- | Current line in input stream.
aiLineL       :: Lens' AlexInput Line
-- | Length of current line.
aiLineLengthL :: Lens' AlexInput Int

aiLineL       = aiIntStoreL . int32L 0  . lineInt32L
aiLineLengthL = aiIntStoreL . int32L 32 . int2Int32L

{-# INLINE takeText #-}
takeText :: AlexInput -> Int -> T.Text
takeText AlexInput{aiPtr} len =
    TE.decodeUtf8 $ utf8BS len aiPtr

countInputSpace :: AlexInput -> Int -> Int
countInputSpace AlexInput{aiPtr} len =
    utf8FoldlBounded len inc 0 aiPtr
    where
    inc !acc ' '#  = acc + 1
    inc !acc '\t'# = acc + 8
    inc !acc c#    = case fixChar c# of
        1## -> acc + 1
        _   -> acc


{-# INLINE withAlexInput #-}
withAlexInput :: C8.ByteString -> (AlexInput -> Int -> a) -> a
withAlexInput s f =
    case s' of
        BSI.PS ptr offset len ->
            BSI.accursedUnutterablePerformIO $ withForeignPtr ptr $ \ptr' -> do
                let !input =
                        set aiLineL initLine $
                        AlexInput
                            { aiPtr      = ptr' `plusPtr` offset
                            , aiIntStore = 0
                            }
                    !res = f input $ len - offset
                touchForeignPtr ptr
                pure res
    where
    -- Line numbering starts from 0 because we're adding additional newline
    -- at the beginning to simplify processing. Thus, line numbers in the
    -- result are 1-based.
    initLine = Line 0

    -- Add '\0' at the end so that we'll find the end of stream (just
    -- as in the old C days...)
    s' = C8.cons '\n' $ C8.snoc (C8.snoc (stripBOM s) '\n') '\0'
    stripBOM :: C8.ByteString -> C8.ByteString
    stripBOM xs
        | "\xEF\xBB\xBF" `C8.isPrefixOf` xs
        = C8.drop 3 xs
        | otherwise
        = xs

{-# INLINE extractDefineOrLetName #-}
extractDefineOrLetName :: AlexInput -> Int -> T.Text
extractDefineOrLetName AlexInput{aiPtr} n =
    TE.decodeUtf8 $ regionToUtf8BS (Ptr start#) end
    where
    !end        = aiPtr `plusPtr` n
    !(Ptr end#) = end
    start#      = (goBack# (end# `plusAddr#` -1#)) `plusAddr#` 1#

    goBack# :: Addr# -> Addr#
    goBack# ptr# = case indexWord8OffAddr# ptr# 0# of
        0##  -> ptr#
        9##  -> ptr# -- '\n'
        10## -> ptr# -- '\n'
        13## -> ptr# -- '\r'
        32## -> ptr# -- ' '
        92## -> ptr# -- '\\'
        _    -> goBack# (ptr# `plusAddr#` -1#)

{-# INLINE dropUntilNL #-}
dropUntilNL :: AlexInput -> AlexInput
dropUntilNL input@AlexInput{aiPtr} =
    input { aiPtr = dropUntilNL# aiPtr }

{-# INLINE dropUntilUnescapedNL #-}
dropUntilUnescapedNL :: AlexInput -> AlexInput
dropUntilUnescapedNL input@AlexInput{aiPtr = start} =
    case dropUntilUnescapedNL# start of
        (# seenNewlines, end #) ->
            over aiLineL (\(Line n) -> Line (n + seenNewlines)) $
            input { aiPtr = end }

{-# INLINE dropUntilNLOr #-}
dropUntilNLOr :: Word8 -> AlexInput -> AlexInput
dropUntilNLOr w input@AlexInput{aiPtr} =
    input { aiPtr = dropUntilNLOr# w aiPtr }

{-# INLINE dropUntilNLOrEither #-}
-- | Drop until either of two bytes.
dropUntilNLOrEither :: Word8 -> Word8 -> AlexInput -> AlexInput
dropUntilNLOrEither w1 w2 input@AlexInput{aiPtr} =
    input { aiPtr = dropUntilNLOrEither# w1 w2 aiPtr }

-- Alex interface

{-# INLINE alexInputPrevChar #-}
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar AlexInput{ aiPtr = Ptr ptr# } =
    case base# `minusAddr#` start# of
        0# -> C# (chr# ch0)
        1# -> let !(# x, _ #) = readChar1# start# ch0 in C# x
        2# -> let !(# x, _ #) = readChar2# start# ch0 in C# x
        3# -> let !(# x, _ #) = readChar3# start# ch0 in C# x
        _  -> '\0' -- Invalid!
    where
    ch0 :: Int#
    !ch0 = word2Int# (indexWord8OffAddr# start# 0#)

    base# = findCharStart ptr# `plusAddr#` -1#

    start# = findCharStart base#

    findCharStart :: Addr# -> Addr#
    findCharStart p#
        | startsWith10# w#
        = findCharStart (p# `plusAddr#` -1#)
        | otherwise
        = p#
        where
        w# = word2Int# (indexWord8OffAddr# p# 0#)

{-# INLINE alexGetByte #-}
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput{aiPtr} =
    case nextChar aiPtr of
        (# c#, n, cs #) ->
            case fixChar c# of
                0##  -> Nothing -- Abort on an unknown character
                -- '\n'
                10## -> Just (10, input')
                    where
                    !input' =
                        over aiLineL increaseLine $
                        set aiLineLengthL 0 $
                        input { aiPtr = cs }
                c    -> Just (b, input')
                    where
                    !b     = W8# c
                    !input' =
                        over aiLineLengthL (+ I# n) $
                        input { aiPtr = cs }

-- Translate unicode character into special symbol we teached Alex to recognize.
{-# INLINE fixChar #-}
fixChar :: Char# -> Word#
fixChar = \case
    -- These should not be translated since Alex knows about them
    '→'#    -> reservedSym
    '∷'#    -> reservedSym
    '⇒'#    -> reservedSym
    '∀'#    -> reservedSym
    '⦇'#    -> reservedSym
    '⦈'#    -> reservedSym
    '⟦'#    -> reservedSym
    '⟧'#    -> reservedSym
    '\x00'# -> fullStop
    '\x01'# -> fullStop
    '\x02'# -> fullStop
    '\x03'# -> fullStop
    '\x04'# -> fullStop
    '\x05'# -> fullStop
    '\x06'# -> fullStop
    '\x07'# -> fullStop
    '\x08'# -> other
    c# -> case ord# c# of
        c2# | isTrue# (c2# <=# 0x7f#) ->
              int2Word# c2# -- Plain ascii needs no fixing.
            | otherwise   ->
                case generalCategory (C# c#) of
                    UppercaseLetter      -> upper
                    LowercaseLetter      -> lower
                    TitlecaseLetter      -> upper
                    ModifierLetter       -> suffix
                    OtherLetter          -> lower
                    NonSpacingMark       -> suffix
                    DecimalNumber        -> digit
                    OtherNumber          -> digit
                    Space                -> space
                    ConnectorPunctuation -> symbol
                    DashPunctuation      -> symbol
                    OtherPunctuation     -> symbol
                    MathSymbol           -> symbol
                    CurrencySymbol       -> symbol
                    ModifierSymbol       -> symbol
                    OtherSymbol          -> symbol

                    SpacingCombiningMark -> space
                    EnclosingMark        -> other
                    LetterNumber         -> symbol
                    OpenPunctuation      -> symbol
                    ClosePunctuation     -> symbol
                    InitialQuote         -> symbol
                    FinalQuote           -> symbol
                    LineSeparator        -> space
                    ParagraphSeparator   -> space
                    Control              -> other
                    Format               -> other
                    Surrogate            -> other
                    PrivateUse           -> other
                    NotAssigned          -> other
    where
      fullStop, space, upper, lower, symbol :: Word#
      digit, suffix, reservedSym, other :: Word#
      fullStop    = 0x00## -- Don't care about these
      space       = 0x01##
      upper       = 0x02##
      lower       = 0x03##
      symbol      = 0x04##
      digit       = 0x05##
      suffix      = 0x06##
      reservedSym = 0x07##
      other       = 0x08##

{-# INLINE unsafeTextHeadAscii #-}
unsafeTextHeadAscii :: Ptr Word8 -> Word8
unsafeTextHeadAscii (Ptr ptr#) = W8# (indexWord8OffAddr# ptr# 0#)

{-# INLINE unsafeTextHeadOfTailAscii #-}
unsafeTextHeadOfTailAscii :: Ptr Word8 -> Word8
unsafeTextHeadOfTailAscii (Ptr ptr#) = W8# (indexWord8OffAddr# ptr# 1#)

{-# INLINE unsafeTextHead #-}
unsafeTextHead :: Ptr Word8 -> Char
unsafeTextHead x =
    case nextChar x of
        (# c#, _, _ #) -> C# c#

{-# INLINE nextChar #-}
nextChar :: Ptr Word8 -> (# Char#, Int#, Ptr Word8 #)
nextChar (Ptr ptr#) =
    case utf8DecodeChar# ptr# of
        (# c#, nBytes# #) -> (# c#, nBytes#, Ptr (ptr# `plusAddr#` nBytes#) #)

{-# INLINE dropUntilNL# #-}
dropUntilNL# :: Ptr Word8 -> Ptr Word8
dropUntilNL# (Ptr start#) = Ptr (go start#)
    where
    go :: Addr# -> Addr#
    go ptr# = case indexWord8OffAddr# ptr# 0# of
        0##  -> ptr#
        10## -> ptr# -- '\n'
        _    -> go (ptr# `plusAddr#` 1#)

{-# INLINE dropUntilUnescapedNL# #-}
dropUntilUnescapedNL# :: Ptr Word8 -> (# Int, Ptr Word8 #)
dropUntilUnescapedNL# (Ptr start#) = go 0 start#
    where
    go :: Int -> Addr# -> (# Int, Ptr Word8 #)
    go !n ptr# = case indexWord8OffAddr# ptr# 0# of
        0##  -> (# n, Ptr ptr# #)
        -- '\n'
        10## -> (# n, Ptr ptr# #)
        -- '\\'
        92## ->
            case indexWord8OffAddr# ptr# 1# of
                0##  -> (# n, Ptr (ptr# `plusAddr#` 1#) #)
                -- '\n'
                10## -> go (n + 1) (ptr# `plusAddr#` 2#)
                _    -> go n (ptr# `plusAddr#` 2#)
        _    -> go n (ptr# `plusAddr#` 1#)

{-# INLINE dropUntilNLOr# #-}
dropUntilNLOr# :: Word8 -> Ptr Word8 -> Ptr Word8
dropUntilNLOr# (W8# w#) (Ptr start#) = Ptr (go start#)
    where
    go :: Addr# -> Addr#
    go ptr# = case indexWord8OffAddr# ptr# 0# of
        0##  -> ptr#
        -- '\n'
        10## -> ptr#
        c# | isTrue# (c# `eqWord#` w#) -> ptr#
           | otherwise                 -> go (ptr# `plusAddr#` 1#)

{-# INLINE dropUntilNLOrEither# #-}
dropUntilNLOrEither# :: Word8 -> Word8 -> Ptr Word8 -> Ptr Word8
dropUntilNLOrEither# (W8# w1#) (W8# w2#) (Ptr start#) = Ptr (go start#)
    where
    go :: Addr# -> Addr#
    go ptr# = case indexWord8OffAddr# ptr# 0# of
        0##  -> ptr#
        -- '\n'
        10## -> ptr#
        c# | isTrue# ((c# `eqWord#` w1#) `orI#` (c# `eqWord#` w2#))
           -> ptr#
           | otherwise
           -> go (ptr# `plusAddr#` 1#)

{-# INLINE utf8Foldl' #-}
utf8Foldl' :: forall a. (a -> Char# -> a) -> a -> Ptr Word8 -> a
utf8Foldl' f x0 (Ptr ptr#) =
    go x0 ptr#
    where
    go :: a -> Addr# -> a
    go !acc addr# =
        case utf8DecodeChar# addr# of
            (# _,  0#      #) -> acc
            (# c#, nBytes# #) -> go (acc `f` c#) (addr# `plusAddr#` nBytes#)

{-# INLINE utf8FoldlBounded #-}
utf8FoldlBounded :: forall a. Int -> (a -> Char# -> a) -> a -> Ptr Word8 -> a
utf8FoldlBounded (I# len#) f x0 (Ptr ptr#) =
    go len# x0 ptr#
    where
    go :: Int#-> a -> Addr# -> a
    go 0# !acc _     = acc
    go n# !acc addr# =
        case utf8DecodeChar# addr# of
            (# _,  0#      #) -> acc
            (# c#, nBytes# #) ->
                go (n# -# 1#) (acc `f` c#) (addr# `plusAddr#` nBytes#)

{-# INLINE utf8BS #-}
utf8BS :: Int -> Ptr Word8 -> BS.ByteString
utf8BS (I# nChars#) (Ptr start#) =
    BSI.PS (BSI.accursedUnutterablePerformIO (newForeignPtr_ (Ptr start#))) 0 (I# (go nChars# 0#))
    where
    go :: Int# -> Int# -> Int#
    go 0# bytes# = bytes#
    go k# bytes# =
        case utf8SizeChar# (start# `plusAddr#` bytes#)  of
            0#      -> bytes#
            nBytes# -> go (k# -# 1#) (bytes# +# nBytes#)

{-# INLINE bytesToUtf8BS #-}
bytesToUtf8BS :: Int -> Ptr Word8 -> BS.ByteString
bytesToUtf8BS (I# nbytes#) (Ptr start#) =
    BSI.PS (BSI.accursedUnutterablePerformIO (newForeignPtr_ (Ptr start#))) 0 (I# nbytes#)

{-# INLINE regionToUtf8BS #-}
regionToUtf8BS :: Ptr Word8 -> Ptr Word8 -> BS.ByteString
regionToUtf8BS start end =
    BSI.PS (BSI.accursedUnutterablePerformIO (newForeignPtr_ start)) 0 (minusPtr end start)

{-# INLINE utf8DecodeChar# #-}
utf8DecodeChar# :: Addr# -> (# Char#, Int# #)
utf8DecodeChar# a# =
    case indexWord8OffAddr# a# 0# of
        0## -> (# '\0'#, 0# #)
        !x# ->
            let !ch0 = word2Int# x# in
            if  | startsWith0# ch0     -> (# chr# ch0, 1# #)
                | startsWith110# ch0   -> readChar1# a# ch0
                | startsWith1110# ch0  -> readChar2# a# ch0
                | startsWith11110# ch0 -> readChar3# a# ch0
                | otherwise            -> invalid# 1#

-- all invalid# sequences end up here:
{-# INLINE invalid# #-}
invalid# :: Int# -> (# Char#, Int# #)
invalid# nBytes# = (# '\8'#, nBytes# #)
-- TODO: check whether following note from ghc applies to server's lexer:
-- '\xFFFD' would be the usual replacement character, but
-- that's a valid symbol in Haskell, so will result in a
-- confusing parse error later on.  Instead we use '\0' which
-- will signal a lexer error immediately.

{-# INLINE readChar1# #-}
readChar1# :: Addr# -> Int# -> (# Char#, Int# #)
readChar1# a# ch0 =
    let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
    if noValidUtf8Cont# ch1 then invalid# 1# else
    (# chr# (((ch0 `andI#` 0x3F#) `uncheckedIShiftL#` 6#) `orI#`
              (ch1 `andI#` 0x7F#)),
       2# #)

{-# INLINE readChar2# #-}
readChar2# :: Addr# -> Int# -> (# Char#, Int# #)
readChar2# a# ch0 =
    let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
    if noValidUtf8Cont# ch1 then invalid# 1# else
    let !ch2 = word2Int# (indexWord8OffAddr# a# 2#) in
    if noValidUtf8Cont# ch2 then invalid# 2# else
    (# chr# (((ch0 `andI#` 0x1F#) `uncheckedIShiftL#` 12#) `orI#`
             ((ch1 `andI#` 0x7F#) `uncheckedIShiftL#` 6#)  `orI#`
              (ch2 `andI#` 0x7F#)),
       3# #)

{-# INLINE readChar3# #-}
readChar3# :: Addr# -> Int# -> (# Char#, Int# #)
readChar3# a# ch0 =
    let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
    if noValidUtf8Cont# ch1 then invalid# 1# else
    let !ch2 = word2Int# (indexWord8OffAddr# a# 2#) in
    if noValidUtf8Cont# ch2 then invalid# 2# else
    let !ch3 = word2Int# (indexWord8OffAddr# a# 3#) in
    if noValidUtf8Cont# ch3 then invalid# 3# else
    (# chr# (((ch0 `andI#` 0x0F#) `uncheckedIShiftL#` 18#) `orI#`
             ((ch1 `andI#` 0x7F#) `uncheckedIShiftL#` 12#) `orI#`
             ((ch2 `andI#` 0x7F#) `uncheckedIShiftL#` 6#)  `orI#`
              (ch3 `andI#` 0x7F#)),
       4# #)

{-# INLINE noValidUtf8Cont# #-}
noValidUtf8Cont# :: Int# -> Bool
noValidUtf8Cont# x = isTrue# ((x <# 0x80#) `orI#` (x ># 0xBF#))

{-# INLINE startsWith0# #-}
startsWith0# :: Int# -> Bool
startsWith0# x = isTrue# ((x `andI#` 0x80#) ==# 0#)

{-# INLINE startsWith10# #-}
startsWith10# :: Int# -> Bool
startsWith10# x = isTrue# ((x `andI#` 0xC0#) ==# 0x80#)

{-# INLINE startsWith110# #-}
startsWith110# :: Int# -> Bool
startsWith110# x = isTrue# ((x `andI#` 0xE0#) ==# 0xC0#)

{-# INLINE startsWith1110# #-}
startsWith1110# :: Int# -> Bool
startsWith1110# x = isTrue# ((x `andI#` 0xF0#) ==# 0xE0#)

{-# INLINE startsWith11110# #-}
startsWith11110# :: Int# -> Bool
startsWith11110# x = isTrue# ((x `andI#` 0xF8#) ==# 0xF0#)

{-# INLINE utf8SizeChar# #-}
utf8SizeChar# :: Addr# -> Int#
utf8SizeChar# a# =
    case indexWord8OffAddr# a# 0# of
        0## -> 0#
        !x# ->
            let !ch0 = word2Int# x# in
            if  | startsWith0# ch0     -> 1#
                | startsWith110# ch0   -> 2#
                | startsWith1110# ch0  -> 3#
                | startsWith11110# ch0 -> 4#
                | otherwise            -> 1#
