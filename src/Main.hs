{-# LANGUAGE OverloadedStrings, PatternGuards, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{- |
    Annotate lines, strip comments, tokenize, then search for
    It loads the existing tags, and updates it for the given file.  Then
    a SaveBuf action runs tags every time you save a file.
    Or extend lushtags, the question is complete parsing ok?  It means I can't
    do it on save, since I'd have to invoke the build system to de-hsc.
-}
module Main where
import qualified Control.Exception as Exception
import Control.Monad
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified Debug.Trace as Trace
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error


main :: IO ()
main = do
    args <- Environment.getArgs
    (flags, inputs) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, inputs, []) -> return (flags, inputs)
        (_, _, errs) -> usage $ "flag errors:\n" ++ List.intercalate ", " errs
    let output = last $ "tags" : [fn | Output fn <- flags]
    oldTags <- fmap (maybe [vimMagicLine] T.lines) $
        catchENOENT $ Text.IO.readFile output
    tags <- fmap concat (mapM processFile inputs)

    let (warnings, newTags) = Either.partitionEithers $ map showTag tags
    forM_ warnings $ \warn -> do
        IO.hPutStrLn IO.stderr warn
    let write = if output == "-" then Text.IO.hPutStr IO.stdout
            else Text.IO.writeFile output

    -- Turns out GHC will not float out the T.pack and it makes a big
    -- performance difference.
    let textFns = map (T.pack . ('\t':)) inputs
        filtered = filter (not . isNewTag textFns) oldTags
    write $ T.unlines $ merge (List.sort newTags) filtered
    where
    usage msg = putStr (GetOpt.usageInfo msg options)
        >> System.Exit.exitSuccess

data Flag = Output FilePath
    deriving (Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option ['o'] [] (GetOpt.ReqArg Output "filename")
        "output file, defaults to 'tags'"
    ]

-- | Documented in vim :h tags-file-format.
-- This tells vim that the file is sorted (but not case folded) so that
-- it can do a bsearch and never needs to fall back to linear search.
vimMagicLine :: Text
vimMagicLine = "!_TAG_FILE_SORTED\t1\t~"

isNewTag :: [Text] -> Text -> Bool
isNewTag textFns line = any (`T.isInfixOf` line) textFns

merge :: [Text] -> [Text] -> [Text]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


-- | Convert a Tag to text, e.g.: AbsoluteMark\tCmd/TimeStep.hs 67 ;" f
showTag :: Tag -> Either String Text
showTag (Pos (SrcPos fn lineno) (Tag text typ)) = Right $
    T.concat [text, "\t", T.pack fn, "\t", T.pack (show lineno), " ;\" ",
        T.singleton (showType typ)]
showTag (Pos pos (Warning text)) = Left $ show pos ++ ": " ++ text

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

data TagVal = Tag !Text !Type | Warning !String
    deriving (Eq, Show)

data Type = Module | Function | Class | Type | Constructor
    deriving (Eq, Show)

data TokenVal = Token !Text | Newline !Int
    deriving (Eq, Show)

type Tag = Pos TagVal
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
    }

instance (Show a) => Show (Pos a) where
    show (Pos pos val) = show pos ++ ":" ++ show val
instance Show SrcPos where
    show (SrcPos fn line) = fn ++ ":" ++ show line

processFile :: FilePath -> IO [Tag]
processFile fn = fmap (process fn) (Text.IO.readFile fn)
    `Exception.catch` \(exc :: Exception.SomeException) -> do
        -- readFile will crash on files that are not UTF8.  Unfortunately not
        -- all haskell source file are.
        IO.hPutStrLn IO.stderr $ "exception reading " ++ show fn ++ ": "
            ++ show exc
        return []

process :: FilePath -> Text -> [Tag]
process fn = dropDups tagText . concatMap blockTags . breakBlocks
    . stripComments . Monoid.mconcat . map tokenize . stripCpp . annotate fn
    where
    tagText (Pos _ (Tag text _)) = text
    tagText (Pos _ (Warning warn)) = T.pack warn

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
        | c == '"' = let (token, rest) = breakString cs
            in Token (T.cons c token) : go rest
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

-- | Break after the ending double-quote of a string.
--
-- TODO \ continuation isn't supported.  I'd have to tokenize at the file
-- level instead of the line level.
breakString :: Text -> (Text, Text)
breakString text = case T.uncons post of
        Nothing -> (text, "")
        Just (c, cs)
            | c == '\\' && T.null cs -> (T.snoc pre c, "")
            | c == '\\' && T.head cs == '"' ->
                let (pre', post') = breakString (T.tail cs)
                in (pre <> "\\\"" <> pre', post')
            | c == '\\' ->
                let (pre', post') = breakString cs
                in (T.snoc pre c <> pre', post')
            | otherwise -> (T.snoc pre c, cs)
    where
    (pre, post) = T.break (\c -> c == '\\' || c == '"') text

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
    stripped -> fst $ functionTags stripped

-- | It's easier to scan for tokens without pesky newlines popping up
-- everywhere.  But I need to keep the newlines in in case I hit a @where@
-- and need to call 'breakBlocks' again.
stripNewlines :: UnstrippedTokens -> [Token]
stripNewlines = filter (not . isNewline) . (\(UnstrippedTokens t) -> t)

-- | Get tags from a function type declaration: token , token , token ::
-- Return the tokens left over.
functionTags :: [Token] -> ([Tag], [Token])
functionTags = go []
    where
    go tags (Pos pos (Token name) : Pos _ (Token "::") : rest)
        | Just name <- functionName name =
            (reverse $ mktag pos name Function : tags, rest)
    go tags (Pos pos (Token name) : Pos _ (Token ",") : rest)
            | Just name <- functionName name =
        go (mktag pos name Function : tags) rest
    go tags tokens = (tags, tokens)

functionName :: Text -> Maybe Text
functionName text
    | isFunction text = Just text
    | isOperator text && not (T.null stripped) = Just stripped
    | otherwise = Nothing
    where
    isFunction text = case T.uncons text of
        Just (c, cs) -> Char.isLower c && startIdentChar c && T.all identChar cs
        Nothing -> False
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
dataTags prevPos unstripped = case dropUntil "=" (stripNewlines unstripped) of
    [] -> [] -- empty data declaration
    Pos pos (Token name) : rest ->
        mktag pos name Constructor : collectRest rest
    rest -> unexpected prevPos unstripped rest "data * ="
    where
    collectRest tokens
        | (tags@(_:_), rest) <- functionTags tokens = tags ++ collectRest rest
    collectRest (Pos _ (Token "|") : Pos pos (Token name) : rest) =
        mktag pos name Constructor : collectRest rest
    collectRest (_ : rest) = collectRest rest
    collectRest [] = []

-- | * => X where X :: * ...
classTags :: SrcPos -> UnstrippedTokens -> [Tag]
classTags prevPos unstripped = case dropContext (stripNewlines unstripped) of
    Pos pos (Token name) : _ ->
        -- Drop the where and start expecting functions.
        mktag pos name Class : concatMap classBodyTags
            (breakBlocks (mapTokens (dropUntil "where") unstripped))
    rest -> unexpected prevPos unstripped rest "class * =>"
    where
    dropContext tokens = if any ((== Token "=>") . valOf) tokens
        then dropUntil "=>" tokens else tokens

classBodyTags :: UnstrippedTokens -> [Tag]
classBodyTags unstripped = case stripNewlines unstripped of
    Pos _ (Token typedata) : Pos pos (Token name) : _
        | typedata `elem` ["type", "data"] -> [mktag pos name Type]
    tokens -> fst $ functionTags tokens


-- * util

mktag :: SrcPos -> Text -> Type -> Pos TagVal
mktag pos name typ = Pos pos (Tag name typ)

warning :: SrcPos -> String -> Pos TagVal
warning pos warn = Pos pos (Warning warn)

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

dropUntil :: Text -> [Token] -> [Token]
dropUntil token = drop 1 . dropWhile ((/= Token token) . valOf)


-- * misc

tracem :: (Show a) => String -> a -> a
tracem msg x = Trace.trace (msg ++ ": " ++ show x) x

(<>) :: (Monoid.Monoid a) => a -> a -> a
(<>) = Monoid.mappend

-- | If @op@ raised ENOENT, return Nothing.
catchENOENT :: IO a -> IO (Maybe a)
catchENOENT op = Exception.handleJust (guard . IO.Error.isDoesNotExistError)
    (const (return Nothing)) (fmap Just op)

dropDups :: (Eq k) => (a -> k) -> [a] -> [a]
dropDups key (x:xs) = go x xs
    where
    go a [] = [a]
    go a (b:bs)
        | key a == key b = go a bs
        | otherwise = a : go b bs
dropDups _ [] = []
