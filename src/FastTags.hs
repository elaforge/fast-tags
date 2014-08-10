{-# LANGUAGE OverloadedStrings, PatternGuards, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module FastTags where
import qualified Control.Exception as Exception
import Control.Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified Debug.Trace as Trace
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error


mergeTags :: [FilePath] -> [Text] -> [Pos TagVal] -> [Text]
mergeTags inputs old new =
    -- 'new' was already been sorted by 'process', but then I just concat
    -- the tags from each file, so they need sorting again.
    merge (map showTag new) (filter (not . isNewTag textFns) old)
    where textFns = Set.fromList $ map T.pack inputs

-- | Documented in vim :h tags-file-format.
-- This tells vim that the file is sorted (but not case folded) so that
-- it can do a bsearch and never needs to fall back to linear search.
vimMagicLine :: Text
vimMagicLine = "!_TAG_FILE_SORTED\t1\t~"

isNewTag :: Set.Set Text -> Text -> Bool
isNewTag textFns line = Set.member fn textFns
    where fn = T.takeWhile (/='\t') $ T.drop 1 $ T.dropWhile (/='\t') line

-- | Convert a Tag to text, e.g.: AbsoluteMark\tCmd/TimeStep.hs 67 ;" f
showTag :: Pos TagVal -> Text
showTag (Pos (SrcPos fn lineno) (Tag text typ)) =
    T.concat [text, "\t", T.pack fn, "\t", T.pack (show lineno), " ;\" ",
        T.singleton (showType typ)]

-- | Vim takes this to be the \"kind:\" annotation.  It's just an arbitrary
-- string and these letters conform to no standard.  Presumably there are some
-- vim extensions that can make use of it.
showType :: Type -> Char
showType typ = case typ of
    Module -> 'm'
    Function -> 'f'
    Class -> 'c'
    Type -> 't'
    Constructor -> 'C'

-- * types

data TagVal = Tag !Text !Type
    deriving (Eq, Show)

data Type = Module | Function | Class | Type | Constructor
    deriving (Eq, Show)

data TokenVal = Token !Text | Newline !Int
    deriving (Eq, Show)

type Tag = Either String (Pos TagVal)
type Token = Pos TokenVal

-- | Newlines have to remain in the tokens because 'breakBlocks' relies on
-- them.  But they make pattern matching on the tokens unreliable because
-- newlines might be anywhere.  A newtype makes sure that the tokens only get
-- stripped once and that I don't do any pattern matching on unstripped tokens.
newtype UnstrippedTokens = UnstrippedTokens [Token]
    deriving (Show, Monoid.Monoid)

mapTokens :: ([Token] -> [Token]) -> UnstrippedTokens -> UnstrippedTokens
mapTokens f (UnstrippedTokens tokens) = UnstrippedTokens (f tokens)

unstrippedTokensOf :: UnstrippedTokens -> [Token]
unstrippedTokensOf (UnstrippedTokens tokens) = tokens

type Line = Pos Text

data Pos a = Pos {
    posOf :: !SrcPos
    , valOf :: !a
    }

data SrcPos = SrcPos {
    posFile :: !FilePath
    , posLine :: !Int
    } deriving (Eq)

instance Show a => Show (Pos a) where
    show (Pos pos val) = show pos ++ ":" ++ show val
instance Show SrcPos where
    show (SrcPos fn line) = fn ++ ":" ++ show line

-- * process

-- | Global processing for when all tags are together.
processAll :: [Pos TagVal] -> [Pos TagVal]
processAll = sortDups . dropDups (\t -> (posOf t, tagText t))
    . sortOn tagText

-- | Given multiple matches, vim will jump to the first one.  So sort adjacent
-- tags with the same text by their type.
--
-- Mostly this is so that given a type with the same name as its module,
-- the type will come first.
sortDups :: [Pos TagVal] -> [Pos TagVal]
sortDups = concat . sort . List.groupBy (\a b -> tagText a == tagText b)
    where
    sort = map (sortOn key)
    key :: Pos TagVal -> Int
    key (Pos _ (Tag _ typ)) = case typ of
        Function -> 0
        Type -> 1
        Constructor -> 2
        Class -> 3
        Module -> 4

tagText :: Pos TagVal -> Text
tagText (Pos _ (Tag text _)) = text

-- | Read tags from one file.
processFile :: FilePath -> IO [Tag]
processFile fn = fmap (process fn) (Text.IO.readFile fn)
    `Exception.catch` \(exc :: Exception.SomeException) -> do
        -- readFile will crash on files that are not UTF8.  Unfortunately not
        -- all haskell source file are.
        IO.hPutStrLn IO.stderr $ "exception reading " ++ show fn ++ ": "
            ++ show exc
        return []

-- | Process one file's worth of tags.
process :: FilePath -> Text -> [Tag]
process fn = concatMap blockTags . breakBlocks . stripComments
    . Monoid.mconcat . map tokenize . stripCpp . annotate fn

-- * tokenize

annotate :: FilePath -> Text -> [Line]
annotate fn text =
    [Pos (SrcPos fn num) line | (num, line) <- zip [1..] (T.lines text)]

-- | Also strips out hsc detritus.
stripCpp :: [Line] -> [Line]
stripCpp = filter $ not . ("#" `T.isPrefixOf`) . valOf

tokenize :: Line -> UnstrippedTokens
tokenize (Pos pos line) = UnstrippedTokens $ map (Pos pos) (tokenizeLine line)

tokenizeLine :: Text -> [TokenVal]
tokenizeLine text = Newline nspaces : go line
    where
    nspaces = T.count " " spaces + T.count "\t" spaces * 8
    (spaces, line) = T.break (not . Char.isSpace) text
    symbols = ["--", "{-", "-}", "=>", "->", "::"]
    go unstripped
        | T.null text = []
        | Just sym <- List.find (`T.isPrefixOf` text) symbols =
            Token sym : go (T.drop (T.length sym) text)
        | c == '\'' = let (token, rest) = breakChar text
            in Token (T.cons c token) : go rest
        | c == '"' = go (skipString cs)
        | (token, rest) <- spanSymbol text, not (T.null token) =
            Token token : go rest
        -- This will tokenize differently than haskell should, e.g.
        -- 9x will be "9x" not "9" "x".  But I just need a wordlike chunk, not
        -- an actual token.  Otherwise I'd have to tokenize numbers.
        | otherwise = case T.span identChar text of
            ("", _) -> Token (T.singleton c) : go cs
            (token, rest) -> Token token : go rest
        where
        text = T.dropWhile Char.isSpace unstripped
        c = T.head text
        cs = T.tail text

startIdentChar :: Char -> Bool
startIdentChar c = Char.isAlpha c || c == '_'

identChar :: Char -> Bool
identChar c = Char.isAlphaNum c || c == '.' || c == '\'' || c == '_'

-- | Span a symbol, making sure to not eat comments.
spanSymbol :: Text -> (Text, Text)
spanSymbol text
    | any (`T.isPrefixOf` post) [",", "--", "-}", "{-"] = (pre, post)
    | Just (c, cs) <- T.uncons post, c == '-' || c == '{' =
        let (pre2, post2) = spanSymbol cs
        in (pre <> T.cons c pre2, post2)
    | otherwise = (pre, post)
    where
    (pre, post) = T.break (\c -> T.any (==c) "-{," || not (symbolChar c)) text

symbolChar :: Char -> Bool
symbolChar c = Char.isSymbol c || Char.isPunctuation c

breakChar :: Text -> (Text, Text)
breakChar text
    | T.null text = ("", "")
    | T.head text == '\\' = T.splitAt 3 text
    | otherwise = T.splitAt 2 text

-- | Skip until the ending double-quote of a string.  Tags never happen in
-- strings so I don't need to keep it.
--
-- TODO \ continuation isn't supported.  I'd have to tokenize at the file
-- level instead of the line level.
skipString :: Text -> Text
skipString text = case T.uncons (T.dropWhile (not . end) text) of
    Nothing -> ""
    Just (c, cs)
        | c == '"' -> cs
        | otherwise -> skipString (T.drop 1 cs)
    where end c = c == '\\' || c == '"'

stripComments :: UnstrippedTokens -> UnstrippedTokens
stripComments = mapTokens (go 0)
    where
    go :: Int -> [Token] -> [Token]
    go _ [] = []
    go nest (pos@(Pos _ token) : rest)
        | token == Token "--" = go nest (dropLine rest)
        | token == Token "{-" = go (nest+1) rest
        | token == Token "-}" = go (nest-1) rest
        | nest > 0 = go nest rest
        | otherwise = pos : go nest rest

-- | Break the input up into blocks based on indentation.
breakBlocks :: UnstrippedTokens -> [UnstrippedTokens]
breakBlocks = map UnstrippedTokens . filter (not . null) . go . filterBlank
    . unstrippedTokensOf
    where
    go [] = []
    go tokens = pre : go post
        where (pre, post) = breakBlock tokens
    -- Blank lines mess up the indentation.
    filterBlank [] = []
    filterBlank (Pos _ (Newline _) : xs@(Pos _ (Newline _) : _)) =
        filterBlank xs
    filterBlank (x:xs) = x : filterBlank xs

-- | Take until a newline, then take lines until the indent established after
-- that newline decreases.
breakBlock :: [Token] -> ([Token], [Token])
breakBlock (t@(Pos _ tok):ts) = case tok of
    Newline indent -> collectIndented indent ts
    _ -> let (pre, post) = breakBlock ts in (t:pre, post)
    where
    collectIndented indent (t@(Pos _ tok) : ts) = case tok of
        Newline n | n <= indent -> ([], t:ts)
        _ -> let (pre, post) = collectIndented indent ts in (t:pre, post)
    collectIndented _ [] = ([], [])
breakBlock [] = ([], [])


-- * extract tags

-- | Get all the tags in one indented block.
blockTags :: UnstrippedTokens -> [Tag]
blockTags tokens = case stripNewlines tokens of
    [] -> []
    Pos _ (Token "module") : Pos pos (Token name) : _ ->
        [mktag pos (snd (T.breakOnEnd "." name)) Module]
    -- newtype X * = X *
    Pos _ (Token "newtype") : Pos pos (Token name) : rest ->
        mktag pos name Type : newtypeTags pos rest
    -- type family X ...
    Pos _ (Token "type") : Pos _ (Token "family") : Pos pos (Token name) : _ ->
        [mktag pos name Type]
    -- type X * = ...
    Pos _ (Token "type") : Pos pos (Token name) : _ -> [mktag pos name Type]
    -- data family X ...
    Pos _ (Token "data") : Pos _ (Token "family") : Pos pos (Token name) : _ ->
        [mktag pos name Type]
    -- data X * = X { X :: *, X :: * }
    -- data X * where ...
    Pos _ (Token "data") : Pos pos (Token name) : _ ->
        mktag pos name Type : dataTags pos (mapTokens (drop 2) tokens)
    -- class * => X where X :: * ...
    Pos pos (Token "class") : _ -> classTags pos (mapTokens (drop 1) tokens)
    -- x, y, z :: *
    stripped -> fst $ functionTags False stripped

-- | It's easier to scan for tokens without pesky newlines popping up
-- everywhere.  But I need to keep the newlines in in case I hit a @where@
-- and need to call 'breakBlocks' again.
stripNewlines :: UnstrippedTokens -> [Token]
stripNewlines = filter (not . isNewline) . (\(UnstrippedTokens t) -> t)

-- | Get tags from a function type declaration: token , token , token ::
-- Return the tokens left over.
functionTags :: Bool -- ^ expect constructors, not functions
    -> [Token] -> ([Tag], [Token])
functionTags constructors = go []
    where
    go tags (Pos pos (Token name) : Pos _ (Token "::") : rest)
        | Just name <- functionName constructors name =
            (reverse $ mktag pos name Function : tags, rest)
    go tags (Pos pos (Token name) : Pos _ (Token ",") : rest)
            | Just name <- functionName constructors name =
        go (mktag pos name Function : tags) rest
    go tags tokens = (tags, tokens)

functionName :: Bool -> Text -> Maybe Text
functionName constructors text
    | isFunction text = Just text
    | isOperator text && not (T.null stripped) = Just stripped
    | otherwise = Nothing
    where
    isFunction text = case T.uncons text of
        Just (c, cs) -> firstChar c && startIdentChar c && T.all identChar cs
        Nothing -> False
    firstChar = if constructors then Char.isUpper else not . Char.isUpper
    -- Technically I could insist on colons if constructors is True, but
    -- let's let ghc decide about the syntax.
    isOperator text = "(" `T.isPrefixOf` text && ")" `T.isSuffixOf` text
        && T.all symbolChar stripped
    stripped = T.drop 1 $ T.take (T.length text - 1) text

-- | * = X *
newtypeTags :: SrcPos -> [Token] -> [Tag]
newtypeTags prevPos tokens = case dropUntil "=" tokens of
    Pos pos (Token name) : _ -> [mktag pos name Constructor]
    rest -> unexpected prevPos (UnstrippedTokens tokens) rest "newtype * ="

-- | [] (empty data declaration)
-- * = X { X :: *, X :: * }
-- * where X :: * X :: *
-- * = X | X
dataTags :: SrcPos -> UnstrippedTokens -> [Tag]
dataTags prevPos unstripped
    | any ((== Token "where") . valOf) (unstrippedTokensOf unstripped) =
        concatMap gadtTags (whereBlock unstripped)
    | otherwise = case dropUntil "=" (stripNewlines unstripped) of
        [] -> [] -- empty data declaration
        Pos pos (Token name) : rest ->
            mktag pos name Constructor : collectRest rest
        rest -> unexpected prevPos unstripped rest "data * ="
    where
    collectRest tokens
        | (tags@(_:_), rest) <- functionTags False tokens =
            tags ++ collectRest rest
    collectRest (Pos _ (Token "|") : Pos pos (Token name) : rest) =
        mktag pos name Constructor : collectRest rest
    collectRest (_ : rest) = collectRest rest
    collectRest [] = []

gadtTags :: UnstrippedTokens -> [Tag]
gadtTags = fst . functionTags True . stripNewlines

-- | * => X where X :: * ...
classTags :: SrcPos -> UnstrippedTokens -> [Tag]
classTags prevPos unstripped = case dropContext (stripNewlines unstripped) of
    Pos pos (Token name) : _ ->
        -- Drop the where and start expecting functions.
        mktag pos name Class : concatMap classBodyTags (whereBlock unstripped)
    rest -> unexpected prevPos unstripped rest "class * =>"
    where
    dropContext tokens
        | any ((== Token "=>") . valOf) (takeUntil "where" tokens) =
            dropUntil "=>" tokens
        | otherwise = tokens

classBodyTags :: UnstrippedTokens -> [Tag]
classBodyTags unstripped = case stripNewlines unstripped of
    Pos _ (Token typedata) : Pos pos (Token name) : _
        | typedata `elem` ["type", "data"] -> [mktag pos name Type]
    tokens -> fst $ functionTags False tokens

-- | Skip to the where and split the indented block below it.
whereBlock :: UnstrippedTokens -> [UnstrippedTokens]
whereBlock = breakBlocks . mapTokens (dropUntil "where")


-- * util

mktag :: SrcPos -> Text -> Type -> Tag
mktag pos name typ = Right $ Pos pos (Tag name typ)

warning :: SrcPos -> String -> Tag
warning pos warn = Left $ show pos ++ ": " ++ warn

unexpected :: SrcPos -> UnstrippedTokens -> [Token] -> String -> [Tag]
unexpected prevPos (UnstrippedTokens tokensBefore) tokensHere declaration =
    [warning pos ("unexpected " ++ thing ++ " after " ++ declaration)]
    where
    thing = if null tokensHere then "end of block"
        else show (valOf (head tokensHere))
    pos
        | not (null tokensHere) = posOf (head tokensHere)
        | not (null tokensBefore) = posOf (last tokensBefore)
        | otherwise = prevPos

dropLine :: [Token] -> [Token]
dropLine = drop 1 . dropWhile (not . isNewline)

isNewline :: Token -> Bool
isNewline (Pos _ (Newline _)) = True
isNewline _ = False

-- | Drop until a token, then drop that token.
dropUntil :: Text -> [Token] -> [Token]
dropUntil token = drop 1 . dropWhile ((/= Token token) . valOf)

-- | Take until, but not including, a token.
takeUntil :: Text -> [Token] -> [Token]
takeUntil token = takeWhile ((/= Token token) . valOf)


-- * misc

tracem :: Show a => String -> a -> a
tracem msg x = Trace.trace (msg ++ ": " ++ show x) x

-- | If @op@ raised ENOENT, return Nothing.
catchENOENT :: IO a -> IO (Maybe a)
catchENOENT op = Exception.handleJust (guard . IO.Error.isDoesNotExistError)
    (const (return Nothing)) (fmap Just op)

dropDups :: Eq k => (a -> k) -> [a] -> [a]
dropDups key (x:xs) = go x xs
    where
    go a [] = [a]
    go a (b:bs)
        | key a == key b = go a bs
        | otherwise = a : go b bs
dropDups _ [] = []

sortOn :: Ord k => (a -> k) -> [a] -> [a]
sortOn key = List.sortBy (\a b -> compare (key a) (key b))

-- | Merge sorted lists.
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
