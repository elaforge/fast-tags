{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module FastTags.Tag (
    -- * types
    TagVal(..)
    , Type(..)
    , Tag(..)
    , Pos(..)
    , SrcPos(..)
    , UnstrippedTokens(..)
    -- * process
    , processFile
    , qualify
    , findSrcPrefix
    , process
    , tokenizeInput
    , processTokens
    -- * util
    , isHsFile
    , defaultModes
    , determineModes
    , ProcessMode(..)

    -- for testing
    , unstrippedTokensOf
    , stripNewlines
    , breakBlocks
    , whereBlock
) where

import Control.Arrow ((***))
import Control.DeepSeq (rnf, NFData)
import Control.Monad

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import Data.Functor ((<$>))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (maybeToList, isJust, fromMaybe)
import Data.Monoid ((<>), Monoid(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Void (Void)

import qualified System.FilePath as FilePath

import FastTags.LexerTypes (LitMode(..))
import qualified FastTags.Lexer as Lexer
import qualified FastTags.Token as Token
import FastTags.Token (Token, Pos(..), SrcPos(..), TokenVal(..))
import qualified FastTags.Util as Util

-- * types

data TagVal = TagVal {
    tvName     :: !Text
    , tvType   :: !Type
    , tvParent :: !(Maybe Text)
      -- ^ parent of this tag; parent can only be of type
      -- Class, Data or Family
    } deriving (Show, Eq, Ord)

tagName :: Pos TagVal -> Text
tagName = tvName . valOf

tagLine :: Pos TagVal -> Token.Line
tagLine = posLine . posOf

instance NFData TagVal where
    rnf (TagVal x y z) = rnf x `seq` rnf y `seq` rnf z

-- | The Ord instance is used to sort tags with the same name.  Given multiple
-- matches, vim will visit them in order, so this should be in the order of
-- interest.
--
-- We rely that Type < Constructor.  TODO how and where?  For sorting tags?
data Type =
    Function
    | Type
    | Constructor
    | Class
    | Module
    | Operator
    | Pattern
    | Family
    | Define -- ^ Preprocessor #define
    deriving (Eq, Ord, Show)

instance NFData Type where
    rnf t = t `seq` ()

data Tag =
    Tag !(Pos TagVal)
    -- | Just like Tag, except these should be deduplicated by their TagVal,
    -- where the one with the lowest line number will be preferred.
    -- The idea seems to be that functions will emit a tag for both the
    -- signature and definition.  TODO seems like a hack, why not just
    -- deduplicate all tags?  And I think I do that now with dropAdjacent.
    | RepeatableTag !(Pos TagVal)
    | Warning !(Pos String)
    deriving (Show, Eq, Ord)

onTagVal :: (Pos TagVal -> Pos TagVal) -> Tag -> Tag
onTagVal f (Tag t)           = Tag $ f t
onTagVal f (RepeatableTag t) = RepeatableTag $ f t
onTagVal _ w@(Warning _)     = w

-- | Partition Tag, RepeatableTag, and Warning.
partitionTags :: [Tag] -> ([Pos TagVal], [Pos TagVal], [Pos String])
partitionTags = go [] [] []
    where
    go tags repeats warns [] = (tags, repeats, reverse warns)
    go tags repeats warns (t:ts) = case t of
        Tag a           -> go (a:tags) repeats warns ts
        RepeatableTag a -> go tags (a:repeats) warns ts
        Warning a       -> go tags repeats (a:warns) ts

extractName :: Tag -> Maybe Text
extractName (Tag t)           = Just $ tagName t
extractName (RepeatableTag t) = Just $ tagName t
extractName (Warning _)       = Nothing

-- | Newlines have to remain in the tokens because 'breakBlocks' relies on
-- them.  But they make pattern matching on the tokens unreliable because
-- newlines might be anywhere.  A newtype makes sure that the tokens only get
-- stripped once and that I don't do any pattern matching on unstripped tokens.
newtype UnstrippedTokens = UnstrippedTokens [Token]
#if MIN_VERSION_base(4,11,0)
    deriving (Show, Semigroup, Monoid)
#else
    deriving (Show, Monoid)
#endif

mapTokens :: ([Token] -> [Token]) -> UnstrippedTokens -> UnstrippedTokens
mapTokens f (UnstrippedTokens tokens) = UnstrippedTokens (f tokens)

unstrippedTokensOf :: UnstrippedTokens -> [Token]
unstrippedTokensOf (UnstrippedTokens tokens) = tokens

-- | Drop @n@ non-newline tokens.
dropTokens :: Int -> UnstrippedTokens -> UnstrippedTokens
dropTokens k = mapTokens (f k)
    where
    f :: Int -> [Token] -> [Token]
    f 0 xs                       = xs
    f _ []                       = []
    f n (Pos _ (Newline _) : xs) = f n xs
    f n (Pos _ _           : xs) = f (n - 1) xs


data ProcessMode
    = ProcessVanilla
      -- ^ LitVanilla Haskell file - everything can produce tags
    | ProcessAlexHappy
      -- ^ Alex/Happy, only first and last braced blocks may produce tags
    deriving (Eq, Ord, Show, Enum, Bounded)

-- * processFile

-- | Read tags from one file.
processFile :: FilePath -> Bool -> IO ([Pos TagVal], [String])
processFile fn trackPrefixes = process fn trackPrefixes <$> BS.readFile fn

-- * qualify

-- | Each tag is split into a one qualified with its module name and one
-- without.
--
-- TODO I could mark it static, to put in a file: mark, which would make vim
-- prioritize it for same-file tags, but I think it already does that, so maybe
-- this isn't necessary?
qualify :: Bool -> Maybe Text -> Pos TagVal -> Pos TagVal
qualify fullyQualify srcPrefix (Token.Pos pos (TagVal name typ _)) =
    Token.Pos pos TagVal
        { tvName   = qualified
        , tvType   = typ
        , tvParent = Nothing
        }
    where
    qualified = case typ of
        Module -> module_
        _ -> module_ <> "." <> name
    module_
        | fullyQualify = T.replace "/" "." $ T.dropWhile (=='/') $
            maybe id dropPrefix srcPrefix $ T.pack file
        | otherwise = T.pack $ FilePath.takeFileName file
    file = FilePath.dropExtension $ Token.posFile pos

dropPrefix :: Text -> Text -> Text
dropPrefix prefix txt = maybe txt id $ T.stripPrefix prefix txt

findSrcPrefix :: [Text] -> Pos a -> Maybe Text
findSrcPrefix prefixes (Token.Pos pos _) =
    List.find (`T.isPrefixOf` file) prefixes
    where file = T.pack $ FilePath.dropExtension $ Token.posFile pos

-- | Process one file's worth of tags.
process :: FilePath -> Bool -> ByteString -> ([Pos TagVal], [String])
process fn trackPrefixes input =
    case tokenizeInput fn trackPrefixes litMode input of
        Left msg   -> ([], [T.unpack msg])
        Right toks -> processTokens procMode toks
    where
    (procMode, litMode) = fromMaybe defaultModes $ determineModes fn

tokenizeInput :: FilePath -> Bool -> LitMode Void -> BS.ByteString
    -> Either Text [Token]
tokenizeInput fn trackPrefixes mode =
    Lexer.tokenize fn mode trackPrefixes

processTokens :: ProcessMode -> [Token] -> ([Pos TagVal], [String])
processTokens mode =
    splitAndRemoveRepeats
    .  concatMap blockTags
    .  breakBlocks mode
    .  UnstrippedTokens
    where
    splitAndRemoveRepeats :: [Tag] -> ([Pos TagVal], [String])
    splitAndRemoveRepeats tags =
        ( earliestRepeats ++ newTags
        , map valOf warnings
        )
        where
        (newTags, repeatableTags, warnings) = partitionTags tags
        -- For RepeatableTag s with duplicate keys, pick the one with the lowest
        -- posLine.
        earliestRepeats :: [Pos TagVal]
        earliestRepeats = Map.elems $ Map.fromListWith minLine $
            Util.keyOn valOf repeatableTags
        minLine x y
            | tagLine x < tagLine y = x
            | otherwise             = y

startIdentChar :: Char -> Bool
startIdentChar '_' = True
startIdentChar c   = Char.isAlpha c

identChar :: Bool -> Char -> Bool
identChar considerDot c = case c of
    '\'' -> True
    '_'  -> True
    '#'  -> True
    '.'  -> considerDot
    c'   -> Char.isAlphaNum c'

isHaskellOp :: Text -> Bool
isHaskellOp str = case Util.headt str of
    Nothing  -> False
    Just ':' -> False
    Just _   -> T.all haskellOpChar str

isHaskellConstructorOp :: Text -> Bool
isHaskellConstructorOp str = case T.uncons str of
    Nothing        -> False
    Just (':', xs) -> T.all haskellOpChar xs
    Just _         -> False

haskellOpChar :: Char -> Bool
haskellOpChar c = case c of
    '_'   -> False
    '-'   -> True
    '!'   -> True
    '#'   -> True
    '$'   -> True
    '%'   -> True
    '&'   -> True
    '*'   -> True
    '+'   -> True
    '.'   -> True
    '/'   -> True
    '<'   -> True
    '='   -> True
    '>'   -> True
    '?'   -> True
    '@'   -> True
    '^'   -> True
    '|'   -> True
    '~'   -> True
    ':'   -> True
    '\\'  -> True
    other -> Util.isSymbolCharacterCategory (Char.generalCategory other)

isTypeVarStart :: Text -> Bool
isTypeVarStart x = case Util.headt x of
    Just c -> Char.isLower c || c == '_'
    _ -> False

-- | Break the input up into blocks based on indentation.
breakBlocks :: ProcessMode -> UnstrippedTokens -> [UnstrippedTokens]
breakBlocks mode
    = map UnstrippedTokens
    . filter (not . null)
    . go
    . stripSemicolonsNotInBraces
    . (case mode of
        ProcessVanilla -> id
        ProcessAlexHappy -> uncurry (++) . firstLastBracedBlock)
    . stripToplevelHscDirectives
    . filterBlank
    . unstrippedTokensOf
    where
    go :: [Token] -> [[Token]]
    go []     = []
    go tokens = pre : go post
        where (pre, post) = breakBlock tokens
    -- Blank lines mess up the indentation.
    filterBlank :: [Token] -> [Token]
    filterBlank [] = []
    filterBlank (Pos _ (Newline _) : xs@(Pos _ (Newline _) : _)) =
        filterBlank xs
    filterBlank (x:xs) = x : filterBlank xs

-- | Collect tokens between toplevel braces. Motivated by Alex/Happy
-- file format that uses braced blocks to separate Haskell source from
-- other directives.
firstLastBracedBlock :: [Token] -> ([Token], [Token])
firstLastBracedBlock tokens =
    (first, last)
    where
    (first, rest) = forward 0 [] tokens
    last          = backward 0 [] $ reverse rest
    forward :: Int -> [Token] -> [Token] -> ([Token], [Token])
    forward  _ acc []                       = (reverse acc, [])
    forward  0 acc (Pos _ LBrace      : ts) = forward 1 acc ts
    forward  0 acc (_                 : ts) = forward 0 acc ts
    forward  1 acc (Pos _ RBrace      : ts) = (reverse acc, ts)
    forward !n acc (t@(Pos _ LBrace)  : ts) = forward (n + 1) (t : acc) ts
    forward !n acc (t@(Pos _ HSCEnum) : ts) = forward (n + 1) (t : acc) ts
    forward !n acc (t@(Pos _ RBrace)  : ts) = forward (n - 1) (t : acc) ts
    forward !n acc (t                 : ts) = forward n (t : acc) ts

    backward :: Int -> [Token] -> [Token] -> [Token]
    backward  _ acc []                       = acc
    backward  0 acc (Pos _ RBrace      : ts) = backward 1 acc ts
    backward  0 acc (_                 : ts) = backward 0 acc ts
    backward  1 acc (Pos _ LBrace      : _)  = acc
    backward !n acc (t@(Pos _ LBrace)  : ts) = backward (n - 1) (t : acc) ts
    backward !n acc (t@(Pos _ HSCEnum) : ts) = backward (n - 1) (t : acc) ts
    backward !n acc (t@(Pos _ RBrace)  : ts) = backward (n + 1) (t : acc) ts
    backward !n acc (t                 : ts) = backward n (t : acc) ts

-- | Take until a newline, then take lines until the indent established after
-- that newline decreases. Or, alternatively, if "{" is encountered then count
-- it as a block until closing "}" is found taking nesting into account.
breakBlock :: [Token] -> ([Token], [Token])
breakBlock = go []
    where
    go :: [Token] -> [Token] -> ([Token], [Token])
    go acc [] = (reverse acc, [])
    go acc (Pos _ Newline{} : t@(Pos _ KWModule) : ts) =
        (reverse acc ++ t : importList, drop 1 rest)
        where
        (importList, rest) = span ((/= KWWhere) . valOf) ts
    go acc (t@(Pos _ tok) : ts) = case tok of
        Newline indent -> collectIndented acc indent ts
        LBrace         -> collectBracedBlock (t : acc) go ts 1
        HSCEnum        -> collectBracedBlock (t : acc) go ts 1
        _              -> go (t : acc) ts

    collectIndented :: [Token] -> Int -> [Token] -> ([Token], [Token])
    collectIndented acc indent = goIndented acc
        where
        goIndented acc' ts' = case ts' of
            Pos _ Newline{} : Pos _ KWModule : _ ->
                (reverse acc', ts')

            []     -> (reverse acc', [])
            t : ts -> case t of
                Pos _ (Newline n) | n <= indent ->
                    (reverse acc', ts')
                Pos _ LBrace ->
                    collectBracedBlock (t : acc') goIndented ts 1
                _ ->
                    goIndented (t : acc') ts

    collectBracedBlock
        :: Show b
        => [Token]
        -> ([Token] -> [Token] -> ([Token], [b]))
        -> [Token]
        -> Int
        -> ([Token], [b])
    collectBracedBlock acc cont = goBraced acc
        where
        goBraced acc' []       _ = (reverse acc', [])
        goBraced acc' ts       0 = cont acc' ts
        goBraced acc' (t : ts) n = goBraced (t : acc') ts $! case t of
            Pos _ LBrace -> n + 1
            Pos _ RBrace -> n - 1
            _            -> n

stripToplevelHscDirectives :: [Token] -> [Token]
stripToplevelHscDirectives = scan
    where
    scan :: [Token] -> [Token]
    scan = \case
        []                            -> []
        Pos _ HSCDirectiveBraced : ts -> skip 1 ts
        t : ts                        -> t : scan ts

    skip :: Int -> [Token] -> [Token]
    skip _  []                              = []
    skip 0  ts                              = scan ts
    skip !n (Pos _ HSCDirectiveBraced : ts) = skip (n + 1) ts
    skip !n (Pos _ LBrace       : ts)       = skip (n + 1) ts
    skip !n (Pos _ RBrace       : ts)       = skip (n - 1) ts
    skip !n (_                  : ts)       = skip n ts

stripSemicolonsNotInBraces :: [Token] -> [Token]
stripSemicolonsNotInBraces =
    go False 0 0
    where
    go  :: Bool -- Whether inside let or where block or case expression
        -> Int -- Indent of last newline
        -> Int -- Parenthesis nesting depth
        -> [Token]
        -> [Token]
    go !_     !_ !_ []                                                       = []
    go !b     !k !n (tok@(Pos _ KWWhere)     : tok'@(Pos _ LBrace) : ts)     = tok : tok' : skipBalancedParens b k (inc n) ts
    go !_     !k !n (tok@(Pos _ KWWhere)     : ts)                           = tok : go True k n ts
    go !b     !k !n (tok@(Pos _ KWLet)       : tok'@(Pos _ LBrace) : ts)     = tok : tok' : skipBalancedParens b k (inc n) ts
    go !_     !k !n (tok@(Pos _ KWLet)       : ts)                           = tok : go True k n ts
    go !b     !k !n (tok@(Pos _ KWDo)        : tok'@(Pos _ LBrace) : ts)     = tok : tok' : skipBalancedParens b k (inc n) ts
    go !_     !k !n (tok@(Pos _ KWDo)        : ts)                           = tok : go True k n ts
    go !b     !k !n (tok@(Pos _ KWOf)        : tok'@(Pos _ LBrace) : ts)     = tok : tok' : skipBalancedParens b k (inc n) ts
    go !_     !k !n (tok@(Pos _ KWOf)        : ts)                           = tok : go True k n ts
    go !_     !k !n (tok@(Pos _ KWIn)        : ts)                           = tok : go False k n ts
    go !_     !_ !n (tok@(Pos _ (Newline k)) : ts)                           = tok : go False k n ts
    go !_     !_  0 (     Pos _ Semicolon    : tok@(Pos _ (Newline k)) : ts) = tok : go False k 0 ts
    go  False !k  0 (     Pos p Semicolon    : ts)                           = Pos p (Newline k) : go False k 0 ts
    go !b     !k !n (tok@(Pos _ LParen)      : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go !b     !k !n (tok@(Pos _ SpliceStart) : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go !b     !k !n (tok@(Pos _ LBracket)    : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go !b     !k !n (tok@(Pos _ LBrace)      : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go !b     !k !n (tok@(Pos _ LBanana)     : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go !b     !k !n (tok@(Pos _ RParen)      : ts)                           = tok : go b k (dec n) ts
    go !b     !k !n (tok@(Pos _ RBracket)    : ts)                           = tok : go b k (dec n) ts
    go !b     !k !n (tok@(Pos _ RBrace)      : ts)                           = tok : go b k (dec n) ts
    go !b     !k !n (tok@(Pos _ RBanana)     : ts)                           = tok : go b k (dec n) ts
    go !b     !k !n (tok : ts)                                               = tok : go b k n       ts

    skipBalancedParens
        :: Bool -- Whether inside where block or after equals sign
        -> Int -- Indent of last newline
        -> Int -- Parenthesis nesting depth
        -> [Token]
        -> [Token]
    skipBalancedParens b k = skip
        where
        skip :: Int -> [Token] -> [Token]
        skip _  []                          = []
        skip 0  ts                          = go b k 0 ts
        skip !n (tok@(Pos _ LParen)      : ts) = tok : skip (inc n) ts
        skip !n (tok@(Pos _ SpliceStart) : ts) = tok : skip (inc n) ts
        skip !n (tok@(Pos _ LBracket)    : ts) = tok : skip (inc n) ts
        skip !n (tok@(Pos _ LBrace)      : ts) = tok : skip (inc n) ts
        skip !n (tok@(Pos _ LBanana)     : ts) = tok : skip (inc n) ts
        skip !n (tok@(Pos _ RParen)      : ts) = tok : skip (dec n) ts
        skip !n (tok@(Pos _ RBracket)    : ts) = tok : skip (dec n) ts
        skip !n (tok@(Pos _ RBrace)      : ts) = tok : skip (dec n) ts
        skip !n (tok@(Pos _ RBanana)     : ts) = tok : skip (dec n) ts
        skip !n (tok : ts)                     = tok : skip n ts

    inc :: Int -> Int
    inc n = n + 1
    dec :: Int -> Int
    dec n = max 0 (n - 1)

explodeToplevelBracedBlocks :: [Token] -> [[Token]]
explodeToplevelBracedBlocks toks =
    case toks of
      Pos _ LBrace : toks' -> filter (not . null) $ go [] 1 toks'
      _                    -> [toks]
    where
    go :: [Token] -> Int -> [Token] -> [[Token]]
    go acc  _   []                          = [reverse acc]
    go acc  0   ts                          = [reverse acc, ts]
    go acc !n   (tok@(Pos _ LBrace)   : ts) = go (tok : acc) (n + 1) ts
    go acc  1   (     Pos _ RBrace    : ts) = reverse acc : go [] 0 ts
    go acc !n   (tok@(Pos _ RBrace)   : ts) = go (tok : acc) (n - 1) ts
    go acc  n@1 (     Pos _ Semicolon : ts) = reverse acc : go [] n ts
    go acc !n   (tok                  : ts) = go (tok : acc) n ts

-- * extract tags

patternRecordFieldNames :: [Token] -> ([Tag], [Token])
patternRecordFieldNames = go []
  where
    go acc ts =
        case ts of
            Pos pos (T name) : rest -> go (mkTag pos name Pattern : acc) rest
            Pos _ Comma      : rest -> go acc rest
            _                       -> (acc, ts)

-- | Get all the tags in one indented block.
-- TODO clean this up to require less nesting, and dropDataContext duplication
blockTags :: UnstrippedTokens -> [Tag]
blockTags unstripped = case stripNewlines unstripped of
    [] -> []
    Pos _ SpliceStart : _ -> []
    Pos _ ToplevelSplice : _ -> []
    Pos pos (CppDefine name) : _ ->
        [mkRepeatableTag pos name Define]
    Pos _ HSCEnum : rest ->
        hsc2hsEnum rest
    Pos _ KWModule : Pos pos (T name) : _ ->
        [mkTag pos (snd (T.breakOnEnd "." name)) Module]
    stripped@(Pos _       (T "pattern") : Pos _ DoubleColon : _) ->
        toplevelFunctionTags stripped
    (Pos _ (T "pattern") : Pos pos (T name) : Pos _ LBrace : rest)
        | (fieldNames, Pos _ RBrace : Pos _ Equals : _) <- patternRecordFieldNames rest ->
        mkTag pos name Pattern : fieldNames
    stripped@(Pos prevPos (T "pattern") : toks) ->
        case tag of
            Nothing -> toplevelFunctionTags stripped
            Just x  -> [x]
        where
        (tag, _, _) = recordVanillaOrInfixName isTypeName Pattern prevPos
            "pattern * =" toks
    Pos _ KWForeign : decl -> foreignTags decl
    -- newtype instance * = ...
    Pos prevPos KWNewtype : Pos _ KWInstance : toks ->
        map (addParent familyNameTag) $ newtypeTags pos $
            dropTokens 2 unstripped
        where
        (familyNameTag, pos) =
            extractFamilyName prevPos "newtype instance * =" toks
    -- newtype X * = X *
    Pos prevPos KWNewtype : toks ->
        maybeToList tag
            ++ map (addParent tag) (newtypeTags pos (dropTokens 1 unstripped))
        where
        (tag, pos, _) =
            recordVanillaOrInfixName isTypeName Type prevPos "newtype * =" toks
    -- type family X ...
    Pos prevPos KWType : Pos _ KWFamily : toks -> maybeToList tag
        where
        (tag, _,  _) = recordVanillaOrInfixName isTypeFamilyName Family prevPos
            "type family * =" toks
    -- type instance X * = ...
    -- No tags in type family instances
    Pos _ KWType : Pos _ KWInstance : _ -> []
    -- type X * = ...
    Pos prevPos KWType : toks
        -- If there’s no equals sign then this is definitely not a type synonym declaration.
        | containsEquals toks
        -> maybeToList tag
        | otherwise
        -> []
        where
        (tag, _, _) = recordVanillaOrInfixName isTypeName Type prevPos
            "type * =" toks
    -- data family X ...
    Pos prevPos KWData : Pos _ KWFamily : toks ->
        map (addParent tag) $ maybeToList tag
        where
        (tag, _, _) = recordVanillaOrInfixName isTypeFamilyName Family prevPos
            "data family * =" toks
    -- data instance * = ...
    -- data instance * where ...
    Pos prevPos KWData : Pos _ KWInstance : toks ->
        map (addParent familyNameTag) $
            dataConstructorTags pos (dropTokens 2 unstripped)
        where
        (familyNameTag, pos) =
            extractFamilyName prevPos "data instance * =" toks
    -- data X * = X { X :: *, X :: * }
    -- data X * where ...
    Pos prevPos KWData : toks ->
        maybeToList tag
            ++ map (addParent tag)
                (dataConstructorTags pos (dropTokens 1 unstripped))
        where
        (tag, pos, _) = recordVanillaOrInfixName isTypeName Type prevPos
            "data * =" toks
    -- class * => X where X :: * ...
    Pos pos KWClass : _ -> classTags pos (dropTokens 1 unstripped)

    Pos _ KWInfix : _ -> []
    Pos _ KWInfixl : _ -> []
    Pos _ KWInfixr : _ -> []
    -- Deriving introduces no new names, just ignore it
    Pos _ KWDeriving : _ -> []
    -- instance * where data * = X :: * ...
    Pos pos KWInstance : _ -> instanceTags pos (dropTokens 1 unstripped)
    -- x, y, z :: *
    stripped -> toplevelFunctionTags stripped

isTypeFamilyName :: Text -> Bool
isTypeFamilyName =
    maybe False (\c -> Char.isUpper c || c == ':') . Util.headt

isTypeName  :: Text -> Bool
isTypeName x = case Util.headt x of
    Just c -> Char.isUpper c || c == ':'
    _ -> False

dropDataContext :: [Token] -> [Token]
dropDataContext = stripParensKindsTypeVars . stripOptContext

recordVanillaOrInfixName
    :: (Text -> Bool)               -- ^ Predicate for names to select
    -> Type                         -- ^ Tope of detecte tag
    -> SrcPos                       -- ^ Previous position to report in errors
    -> String                       -- ^ Context to report in errors
    -> [Token]                      -- ^ Tokens to analyze
    -> (Maybe Tag, SrcPos, [Token]) -- ^ Possibly detected tag and rest of the tokens
recordVanillaOrInfixName isVanillaName tokenType prevPos context tokens =
    case dropDataContext tokens of
        toks | Type <- tokenType
             , Just (pos, name, rest) <- extractSpecialTypeName toks ->
            (Just $ mkTag pos name tokenType, pos, rest)
        Pos _ RParen   : _                -> (Nothing, prevPos, tokens)
        Pos _ LBracket : _                -> (Nothing, prevPos, tokens)
        Pos _ Equals   : _                -> (Nothing, prevPos, tokens)
        Pos _ Comma    : _                -> (Nothing, prevPos, tokens)
        tok : toks ->
            case tok of
                Pos pos (tokToName -> Just name) | isVanillaName name ->
                    (Just $ mkTag pos name tokenType, pos, toks)
                _ -> case dropInfixTypeStart $ tok : toks of
                    Pos pos (tokToName -> Just name) : rest ->
                        (Just $ mkTag pos name tokenType, pos, rest)
                    rest -> (Just $ unexp pos rest, pos, tok : toks)
                        where pos = posOf tok
        [] -> (Just $ unexp prevPos [], prevPos, [])
    where
    unexp pos rest = unexpected pos (UnstrippedTokens tokens) rest context

extractSpecialTypeName :: [Token] -> Maybe (SrcPos, Text, [Token])
extractSpecialTypeName (Pos pos LBracket : Pos _ RBracket : rest) = Just (pos, "[]", rest)
extractSpecialTypeName (Pos pos LParen   : (tupleCommas -> (commas, Pos _ RParen : rest))) =
    Just (pos, "(" <> T.replicate commas "," <> ")", rest)
extractSpecialTypeName (tupleCommas -> (commas, Pos pos RParen : rest)) =
    Just (pos, "(" <> T.replicate commas "," <> ")", rest)
extractSpecialTypeName _ = Nothing

tupleCommas :: [Token] -> (Int, [Token])
tupleCommas = go 0 True
    where
    go :: Int -> Bool -> [Token] -> (Int, [Token])
    go !n False (Pos _ Comma : rest) = go (n + 1) True rest
    go !n False rest                 = (n, rest)
    go !n True  (Pos _ Comma : rest) =
        go (n + 1) True rest
    go !n True  rest'@(Pos _ (T name) : rest)
        | isTypeVarStart name = go n False rest
        | otherwise           = (n, rest')
    go !n _     rest = (n, rest)

-- same as dropWhile with counting
dropInfixTypeStart :: [Token] -> [Token]
dropInfixTypeStart = dropWhile f
    where
    f (Pos _ (T name)) = isInfixTypePrefix name
    f (Pos _ Backtick) = True
    f (Pos _ LParen)   = True
    f _                = False

    isInfixTypePrefix :: Text -> Bool
    isInfixTypePrefix = maybe False Char.isLower . Util.headt

-- | It's easier to scan for tokens without pesky newlines popping up
-- everywhere.  But I need to keep the newlines in in case I hit a @where@
-- and need to call 'breakBlocks' again.
stripNewlines :: UnstrippedTokens -> [Token]
stripNewlines = filter (not . isNewline) . unstrippedTokensOf

-- | hsc2hs's '#enum ... \n' or '#{enum...}' definition.
hsc2hsEnum :: [Token] -> [Tag]
hsc2hsEnum = \case
    _ : Pos _ Comma : _ : Pos _ Comma : rest -> extractValues rest
    _ -> []
    where
    -- Values are not really functions, they're constants like x = 0 but there's
    -- no tag type for that.
    valueTyp = Function
    extractValues :: [Token] -> [Tag]
    extractValues = \case
        Pos _ Comma : rest ->
            extractValues rest
        Pos p (T name) : Pos _ Equals : rest ->
            mkTag p name valueTyp
                : extractValues (dropUntil Comma (stripBalancedParens rest))
        Pos p (T name) : rest ->
            mkTag p (translateName name) valueTyp : extractValues rest
        _ -> []
    translateName :: Text -> Text
    translateName
        = TL.toStrict
        . TLB.toLazyText
        . snd
        . T.foldl' addChar (False, mempty)
    addChar :: (Bool, TLB.Builder) -> Char -> (Bool, TLB.Builder)
    addChar (_, acc) '_' = (True, acc)
    addChar (b, acc) c   = (False, acc <> TLB.singleton c')
        where
        c' = if b then Char.toUpper c else Char.toLower c

-- | Tags from foreign import.
--
-- e.g. @foreign import ccall safe \"name\" c_name :: ...@ will produce a tag
-- for @c_name@.
foreignTags :: [Token] -> [Tag]
foreignTags decl = case decl of
    Pos _ KWImport : decl'
        | Pos pos (T name) : _ <- Util.dropBefore isDoubleColon decl' ->
            [mkTag pos name Function]
    _ -> []
    where
    isDoubleColon (Pos _ DoubleColon) = True
    isDoubleColon _ = False

toplevelFunctionTags :: [Token] -> [Tag]
toplevelFunctionTags toks = case tags of
    -- Tags of toplevel functions are all repeatable, even the ones that come
    -- from the type signature because there will definitely be tags from the
    -- body and they should be sorted out if type signature is present.
    [] -> functionTagsNoSig toks
    ts -> map toRepeatableTag ts
    where
    -- first try to detect tags from type signature, if it fails then
    -- do the actual work of detecting from body
    (tags, _) = functionTags ExpectFunctions toks
    toRepeatableTag :: Tag -> Tag
    toRepeatableTag (Tag t) = RepeatableTag t
    toRepeatableTag t       = t

functionTagsNoSig :: [Token] -> [Tag]
functionTagsNoSig allToks
    -- If there’s no equals sign then this is definitely not a function/operator declaration.
    | containsEquals allToks
    = go' allToks
    | otherwise
    = []
    where
    go' :: [Token] -> [Tag]
    go' (Pos _ T{} : Pos pos tok : _)
        | Just opName <- tokToOpNameExcludingBangPatSyms ExpectFunctions tok
        = [mkRepeatableTag pos opName Operator]
    go' ts = go ts

    go :: [Token] -> [Tag]
    go []                           = []
    go (Pos _ LParen : Pos _ T{} : Pos _ Backtick : Pos pos' (T name')
            : Pos _ Backtick : Pos _ T{} : Pos _ RParen : _)
        | functionName ExpectFunctions name' =
            [mkRepeatableTag pos' name' Function]
    go (Pos _ LParen : Pos _ T{} : Pos pos' tok : Pos _ T{} : Pos _ RParen : _)
        | Just name' <- tokToOpName ExpectFunctions tok
        = [mkRepeatableTag pos' name' Operator]
    go toks@(Pos _ LParen : _)      = go $ stripBalancedParens toks
    go toks@(Pos _ LBrace : _)      = go $ stripBalancedBraces toks
    go toks@(Pos _ LBracket : _)    = go $ stripBalancedBrackets toks
    -- This function does not analyze type signatures.
    go (Pos _ DoubleColon : _)      = []
    go (Pos _ ExclamationMark : ts) = go ts
    go (Pos _ Tilde : ts)           = go ts
    go (Pos _ At : ts)              = go ts
    go (Pos _ Equals : _)           = functionOrOp allToks
    go (Pos _ Pipe : _)             = functionOrOp allToks
    go (Pos _ Backtick : Pos pos' (T name') : _)
        | functionName ExpectFunctions name' =
            [mkRepeatableTag pos' name' Function]
    go (Pos pos tok : _)
        | Just name <- tokToOpNameExcludingBangPatSyms ExpectFunctions tok
        = [mkRepeatableTag pos name Operator]
    go (Pos pos Dot : _)            = [mkRepeatableTag pos "." Operator]
    go (_ : ts)                     = go ts
    stripOpeningParens :: [Token] -> [Token]
    stripOpeningParens = dropWhile ((== LParen) . valOf)
    functionOrOp :: [Token] -> [Tag]
    functionOrOp toks = case stripOpeningParens toks of
         Pos pos (T name) : _
             | functionName ExpectFunctions name ->
                [mkRepeatableTag pos name Function]
         Pos pos tok : _ -> case tokToOpName ExpectFunctions tok of
             Just name -> [mkRepeatableTag pos name Operator]
             Nothing   -> []
         [] -> []

tokToOpNameExcludingBangPatSyms :: ExpectedFuncName -> TokenVal -> Maybe Text
tokToOpNameExcludingBangPatSyms expectation tok =
    case (expectation, tokToNameExcludingBangPatSyms tok) of
        (ExpectFunctions, res@(Just name))
            | isHaskellOp name -> res
        (ExpectConstructors, res@(Just name))
            | isHaskellConstructorOp name -> res
        _ -> Nothing

tokToNameExcludingBangPatSyms :: TokenVal -> Maybe Text
tokToNameExcludingBangPatSyms (T "_")         = Nothing
tokToNameExcludingBangPatSyms (T name)        = Just name
tokToNameExcludingBangPatSyms Dot             = Just "."
tokToNameExcludingBangPatSyms _               = Nothing

tokToOpName :: ExpectedFuncName -> TokenVal -> Maybe Text
tokToOpName expectation tok = case (expectation, tokToName tok) of
    (ExpectFunctions, res@(Just name))
        | isHaskellOp name -> res
    (ExpectConstructors, res@(Just name))
        | isHaskellConstructorOp name -> res
    _ -> Nothing

tokToName :: TokenVal -> Maybe Text
tokToName ExclamationMark = Just "!"
tokToName Tilde           = Just "~"
tokToName x               = tokToNameExcludingBangPatSyms x

-- | Get tags from a function type declaration: token , token , token ::
-- Return the tokens left over.
functionTags :: ExpectedFuncName -- ^ expect constructors or functions
    -> [Token] -> ([Tag], [Token])
functionTags constructors = go []
    where
    (opTag, funcTag) = case constructors of
        ExpectConstructors -> (Constructor, Constructor)
        ExpectFunctions    -> (Operator, Function)
    go :: [Tag] -> [Token] -> ([Tag], [Token])
    go tags (Pos _ LParen : opTok : Pos _ RParen : Pos _ DoubleColon : rest) =
        (reverse $ mkOpTag tags opTag opTok, rest)
    go tags (Pos pos (T name) : Pos _ DoubleColon : rest)
        | functionName constructors name =
            (reverse $ mkTag pos name funcTag : tags, rest)
    go tags (Pos _ LParen : opTok : Pos _ RParen : Pos _ Comma : rest) =
        go (mkOpTag tags opTag opTok) rest
    go tags (Pos pos (T name) : Pos _ Comma : rest)
        | functionName constructors name =
            go (mkTag pos name funcTag : tags) rest
    go tags tokens = (tags, tokens)

    mkOpTag :: [Tag] -> Type -> Token -> [Tag]
    mkOpTag tags opTag' (Pos pos tok) =
        case tokToOpName constructors tok of
            Just name -> mkTag pos name opTag' : tags
            Nothing   -> tags

data ExpectedFuncName = ExpectFunctions | ExpectConstructors

functionName :: ExpectedFuncName -> Text -> Bool
functionName expect = isFunction
    where
    isFunction text = case T.uncons text of
        Just ('_', cs)
            | T.null cs -> False
        Just (c, cs) ->
            firstChar c && startIdentChar c && T.all (identChar True) cs
        Nothing      -> False
    firstChar = case expect of
        ExpectFunctions    -> \c -> Char.isLower c || c == '_'
        ExpectConstructors -> Char.isUpper

-- | * = X *
newtypeTags :: SrcPos -> UnstrippedTokens -> [Tag]
newtypeTags _ unstripped
    | any (\case { Pos _ KWWhere -> True; _ -> False })
            (unstrippedTokensOf unstripped) =
        concatMap gadtTags (whereBlock unstripped)
newtypeTags prevPos unstripped =
    case dropUntil Equals $ stripNewlines unstripped of
        Pos pos (T name) : rest ->
            let constructor = mkTag pos name Constructor
            in  case rest of
                Pos _ LBrace : Pos funcPos (T funcName) : _ ->
                    [constructor, mkTag funcPos funcName Function]
                _ ->
                    [constructor]
        rest -> [unexpected prevPos unstripped rest "newtype * ="]

-- | [] (empty data declaration)
-- * = X { X :: *, X :: * }
-- * where X :: * X :: *
-- * = X | X
dataConstructorTags :: SrcPos -> UnstrippedTokens -> [Tag]
dataConstructorTags prevPos unstripped
    -- GADT
    | any (\case { Pos _ KWWhere -> True; _ -> False })
            (unstrippedTokensOf unstripped) =
        concatMap gadtTags (whereBlock unstripped)
    -- plain ADT
    | otherwise = case strip unstripped of
        [] -> [] -- empty data declaration
        rest | Just (Pos pos (T name), rest') <- extractInfixConstructor rest ->
            mkTag pos name Constructor : collectRest rest'
        rest | Just (pos, name, rest') <- extractSpecialTypeName rest ->
            mkTag pos name Constructor : collectRest rest'
        Pos pos (T name) : rest ->
            mkTag pos name Constructor : collectRest rest
        Pos _ LParen : Pos pos (T name) : Pos _ RParen : rest ->
            mkTag pos name Constructor : collectRest rest
        rest -> [unexpected prevPos unstripped rest "data * = *"]
    where
    strip :: UnstrippedTokens -> [Token]
    strip = stripOptBang . stripDatatypeContext . dropUntil Equals
          . stripNewlines
    collectRest :: [Token] -> [Tag]
    collectRest tokens
        | (tags@(_:_), rest) <- functionTags ExpectFunctions tokens =
            tags ++ collectRest (dropUntilNextField rest)
    collectRest toks@(Pos _ LParen : _) =
        collectRest $ stripBalancedParens toks -- dropUntilNextField rest
    collectRest (Pos pipePos Pipe : rest)
        | Just (Pos pos (T name), rest'') <- extractInfixConstructor rest' =
            mkTag pos name Constructor : collectRest rest''
        | Just (pos, name, rest'') <- extractSpecialTypeName rest' =
            mkTag pos name Constructor : collectRest rest''
        | Pos pos (T name) : rest'' <- rest'
        , functionName ExpectConstructors name =
            mkTag pos name Constructor
                : collectRest (dropUntilNextCaseOrRecordStart rest'')
        | Pos _ LParen : Pos pos (T name) : Pos _ RParen : rest'' <- rest'
        , isHaskellConstructorOp name =
            mkTag pos name Constructor
                : collectRest (dropUntilNextCaseOrRecordStart rest'')
        | otherwise =
            [unexpected pipePos unstripped rest "| not followed by tokens"]
        where
        rest' = stripOptBang $ stripDatatypeContext rest
    collectRest (_ : rest) = collectRest rest
    collectRest [] = []

    stripOptBang :: [Token] -> [Token]
    stripOptBang (Pos _ ExclamationMark : rest) = rest
    stripOptBang ts = ts

    extractInfixConstructor :: [Token] -> Maybe (Token, [Token])
    extractInfixConstructor = extract . stripTypeParam
        where
        extract :: [Token] -> Maybe (Token, [Token])
        extract (tok@(Pos _ (T name)) : rest)
            | isHaskellConstructorOp name = Just (tok, stripTypeParam rest)
        extract (Pos _ Backtick : tok@(Pos _ _) : Pos _ Backtick : rest) =
            Just (tok, stripTypeParam rest)
        extract _ = Nothing

        stripTypeParam :: [Token] -> [Token]
        stripTypeParam input@(Pos _ LParen : _) =
            stripBalancedParens input
        stripTypeParam input@(Pos _ LBracket : _) =
            stripBalancedBrackets input
        stripTypeParam ts = dropWhile isTypeParam $ drop 1 ts

        isTypeParam :: Token -> Bool
        isTypeParam (Pos _ (T name)) = isTypeVarStart name
        isTypeParam _                = False

    dropUntilNextCaseOrRecordStart :: [Token] -> [Token]
    dropUntilNextCaseOrRecordStart = dropWithStrippingBalanced $
        not . \case { Pipe -> True; LBrace -> True; _ -> False }

    dropUntilNextField :: [Token] -> [Token]
    dropUntilNextField = dropWithStrippingBalanced $
        not . \case { Comma -> True; RBrace -> True; Pipe -> True; _ -> False }

stripDatatypeContext :: [Token] -> [Token]
stripDatatypeContext = stripOptContext . stripOptForall

stripOptForall :: [Token] -> [Token]
stripOptForall (Pos _ (T "forall") : rest) = dropUntil Dot rest
stripOptForall xs                          = xs

stripParensKindsTypeVars :: [Token] -> [Token]
stripParensKindsTypeVars (Pos _ LParen : xs)  =
    stripParensKindsTypeVars xs
stripParensKindsTypeVars (Pos _ DoubleColon : xs) =
    stripParensKindsTypeVars $ drop 1 $
    dropWithStrippingBalanced (\case { RParen -> False; _ -> True }) xs
stripParensKindsTypeVars (Pos _ (T name) : xs)
    | isTypeVarStart name = stripParensKindsTypeVars xs
stripParensKindsTypeVars xs = xs

stripOptContext :: [Token] -> [Token]
stripOptContext (stripBalancedParens -> Pos _ Implies : xs) = xs
stripOptContext origToks = go origToks
    where
    go (Pos _ Implies : xs)    = xs
    go (Pos _ Equals : _)      = origToks
    go (Pos _ Pipe : _)        = origToks
    go (Pos _ LBrace : _)      = origToks
    go (Pos _ RBrace : _)      = origToks
    go toks@(Pos _ LParen : _) = go $ stripBalancedParens toks
    go (Pos _ DoubleColon : _) = origToks
    go (_ : xs)                = go xs
    go []                      = origToks

-- | Drop all tokens for which @pred@ returns True, also drop () or []
-- parenthesized expressions.
dropWithStrippingBalanced :: (TokenVal -> Bool) -> [Token] -> [Token]
dropWithStrippingBalanced p = go
    where
    go input@(Pos _ LParen : _)   = go $ stripBalancedParens input
    go input@(Pos _ LBracket : _) = go $ stripBalancedBrackets input
    go (Pos _ tok : xs) | p tok   = go xs
    go xs = xs

stripBalancedParens :: [Token] -> [Token]
stripBalancedParens = stripBalanced LParen RParen

stripBalancedBrackets :: [Token] -> [Token]
stripBalancedBrackets = stripBalanced LBracket RBracket

stripBalancedBraces :: [Token] -> [Token]
stripBalancedBraces = stripBalanced LBrace RBrace

stripBalanced :: TokenVal -> TokenVal -> [Token] -> [Token]
stripBalanced open close (Pos _ tok : xs)
    | tok == open = go 1 xs
    where
    go :: Int -> [Token] -> [Token]
    go 0 ys = ys
    go !n (Pos _ tok' : ys)
        | tok' == open  = go (n + 1) ys
        | tok' == close = go (n - 1) ys
    go !n (_: ys) = go n ys
    go _  []      = []
stripBalanced _ _ xs = xs

gadtTags :: UnstrippedTokens -> [Tag]
gadtTags unstripped = case dropDataContext rest of
    Pos _ LBrace : rest' -> constructorTag ++ collectFields rest'
    _                    -> constructorTag
    where
    (constructorTag, rest) =
        functionTags ExpectConstructors $ stripNewlines unstripped
    collectFields :: [Token] -> [Tag]
    collectFields (Pos _ Comma : rest) = collectFields rest
    collectFields (Pos _ RBrace : _)   = []
    collectFields tokens
        | (tags@(_:_), rest) <- functionTags ExpectFunctions tokens =
            tags ++ collectFields (dropUntilNextField rest)
        | otherwise = []
    dropUntilNextField :: [Token] -> [Token]
    dropUntilNextField = dropWithStrippingBalanced $
        not . \case { Comma -> True; RBrace -> True; _ -> False }

-- | * => X where X :: * ...
classTags :: SrcPos -> UnstrippedTokens -> [Tag]
classTags prevPos unstripped =
    maybeToList classTag
        ++ map (addParent classTag)
            (concatMap classBodyTags (whereBlock wherePart))
    where
    (classPart, wherePart) = spanUntil KWWhere unstripped
    (classTag, _, _) = recordVanillaOrInfixName isTypeName Class prevPos
        "class * =>" $ stripUntilImplies $ stripNewlines classPart

stripUntilImplies :: [Token] -> [Token]
stripUntilImplies xs = case dropUntil Implies xs of
    []  -> xs
    xs' -> xs'

classBodyTags :: UnstrippedTokens -> [Tag]
classBodyTags unstripped = case stripNewlines unstripped of
    Pos _ KWType : Pos pos (T name) : _ -> [mkTag pos name Family]
    Pos _ KWData : Pos pos (T name) : _ -> [mkTag pos name Family]
    tokens -> fst $ functionTags ExpectFunctions tokens

-- | Skip to the where and split the indented block below it.
whereBlock :: UnstrippedTokens -> [UnstrippedTokens]
whereBlock =
    concatMap (breakBlocks ProcessVanilla . UnstrippedTokens) .
    explodeToplevelBracedBlocks .
    dropUntil KWWhere .
    unstrippedTokensOf

instanceTags :: SrcPos -> UnstrippedTokens -> [Tag]
instanceTags prevPos unstripped =
    -- instances can offer nothing but some fresh data constructors since
    -- the actual datatype is really declared in the class declaration
    concatMap newtypeDecl (map (dropTokens 1) (filter isNewtypeDecl block))
    ++ concatMap dataDecl (map (dropTokens 1) (filter isDataDecl block))
    where
    newtypeDecl toks = map (addParent parent) $ newtypeTags pos toks
        where
        (parent, pos) = extractFamilyName prevPos "newtype instance * ="
            (stripNewlines toks)
    dataDecl toks = map (addParent parent) $ dataConstructorTags pos toks
        where
        (parent, pos) = extractFamilyName prevPos "data instance * ="
            (stripNewlines toks)
    block = whereBlock unstripped

    isNewtypeDecl :: UnstrippedTokens -> Bool
    isNewtypeDecl (UnstrippedTokens (Pos _ KWNewtype : _)) = True
    isNewtypeDecl _ = False

    isDataDecl :: UnstrippedTokens -> Bool
    isDataDecl (UnstrippedTokens (Pos _ KWData : _)) = True
    isDataDecl _ = False

extractFamilyName :: SrcPos -> String -> [Token] -> (Maybe Tag, SrcPos)
extractFamilyName prevPos context toks = (tag, pos)
    where
    (tag, pos, _) = recordVanillaOrInfixName isTypeFamilyName Family prevPos
        context toks

-- * util

addParent :: Maybe Tag -> Tag -> Tag
addParent parent = onTagVal f
    where
    f (Pos pos (TagVal name typ _)) =
        Pos pos (TagVal name typ parentName)
    parentName :: Maybe Text
    parentName = join $ extractName <$> parent

mkTag :: SrcPos -> Text -> Type -> Tag
mkTag pos name typ = Tag $ Pos pos (TagVal name typ Nothing)

mkRepeatableTag :: SrcPos -> Text -> Type -> Tag
mkRepeatableTag pos name typ =
    RepeatableTag $ Pos pos TagVal
        { tvName   = name
        , tvType   = typ
        , tvParent = Nothing
        }

warning :: SrcPos -> String -> Tag
warning pos warn = Warning $ Pos pos $ show pos ++ ": " ++ warn

unexpected :: SrcPos -> UnstrippedTokens -> [Token] -> String -> Tag
unexpected prevPos (UnstrippedTokens tokensBefore) tokensHere declaration =
    warning pos ("unexpected " ++ thing ++ " after " ++ declaration)
    where
    thing = maybe "end of block" (show . valOf) (Util.mhead tokensHere)
    pos
        | Just t <- Util.mhead tokensHere = posOf t
        | Just t <- Util.mlast tokensBefore = posOf t
        | otherwise = prevPos

isNewline :: Token -> Bool
isNewline (Pos _ (Newline _)) = True
isNewline _                   = False

containsEquals :: [Token] -> Bool
containsEquals = any (\case { Pos _ Equals -> True; _ -> False; })

dropUntil :: TokenVal -> [Token] -> [Token]
dropUntil token = drop 1 . dropWhile (not . (== token) . valOf)

spanUntil :: TokenVal -> UnstrippedTokens
    -> (UnstrippedTokens, UnstrippedTokens)
spanUntil token
    = (UnstrippedTokens *** UnstrippedTokens)
    . span (not . (== token) . valOf)
    . unstrippedTokensOf

-- | Crude predicate for Haskell files
isHsFile :: FilePath -> Bool
isHsFile = isJust . determineModes

defaultModes :: (ProcessMode, LitMode Void)
defaultModes = (ProcessVanilla, LitVanilla)

determineModes :: FilePath -> Maybe (ProcessMode, LitMode Void)
determineModes x = case FilePath.takeExtension x of
    ".hs"  -> Just defaultModes
    ".hsc" -> Just defaultModes
    ".lhs" -> Just (ProcessVanilla, LitOutside)
    ".x"   -> Just (ProcessAlexHappy, LitVanilla)
    ".y"   -> Just (ProcessAlexHappy, LitVanilla)
    ".lx"  -> Just (ProcessAlexHappy, LitOutside)
    ".ly"  -> Just (ProcessAlexHappy, LitOutside)
    _      -> Nothing
