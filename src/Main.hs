{-# LANGUAGE OverloadedStrings, PatternGuards #-}
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
    write $ T.unlines $
        merge (List.sort newTags) (filter (not . isNewTag inputs) oldTags)
    where
    usage msg = putStr (GetOpt.usageInfo msg options)
        >> System.Exit.exitSuccess

data Flag = Output FilePath
    deriving (Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option ['o'] [] (GetOpt.ReqArg Output "blorch")
        "output file, defaults to 'tags'"
    ]

-- | Documented in vim :h tags-file-format.
-- This tells vim that the file is sorted (but not case folded) so that
-- it can do a bsearch and never needs to fall back to linear search.
vimMagicLine :: Text
vimMagicLine = "!_TAG_FILE_SORTED\t1\t~"

isNewTag :: [FilePath] -> Text -> Bool
isNewTag fns line = any (`T.isInfixOf` line) textFns
    where textFns = map (T.pack . ('\t':)) fns

merge :: [Text] -> [Text] -> [Text]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


-- AbsoluteMark\tCmd/TimeStep.hs 67 ;" f
-- TODO use "file:" for non-exported symbols, this should make vim search for
-- them first within the file.
showTag :: Tag -> Either String Text
showTag (Pos (SrcPos fn lineno) (Tag text typ)) = Right $
    T.concat [text, "\t", T.pack fn, "\t", T.pack (show lineno), " ;\" ",
        T.singleton (showType typ)]
showTag (Pos pos (Warning text)) = Left $ show pos ++ ": " ++ text

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
type Line = Pos Text

data Pos a = Pos {
    posOf :: SrcPos
    , valOf :: a
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

process :: FilePath -> Text -> [Tag]
process fn = dropDups tagText . concatMap blockTags . breakBlocks
    . stripComments . concatMap tokenize . stripCpp . annotate fn
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

tokenize :: Line -> [Token]
tokenize (Pos pos line) = map (Pos pos) (tokenizeLine line)

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
        | Char.isSymbol c || Char.isPunctuation c =
            Token (T.singleton c) : go cs
        | otherwise =
            let (token, rest) = T.span isIdent text in Token token : go rest
        where
        text = T.dropWhile Char.isSpace unstripped
        c = T.head text
        cs = T.tail text
    isIdent c = Char.isAlphaNum c || c == '.' || c == '\'' || c == '_'

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

stripComments :: [Token] -> [Token]
stripComments = go 0
    where
    go :: Int -> [Token] -> [Token]
    go _ [] = []
    go nest (pos@(Pos _ token) : rest)
        | token == Token "--" = go nest (dropLine rest)
        | token == Token "{-" = go (nest+1) rest
        | token == Token "-}" = go (nest-1) rest
        | nest > 0 = go nest rest
        | otherwise = pos : go nest rest


-- | Break the input up into blocks based on indentation and strip out all
-- the newlines.  This way the blockTags doesn't need to worry about newlines.
breakBlocks :: [Token] -> [[Token]]
breakBlocks =
    filter (not . null) . map (filter (not . isNewline)) . go . filterBlank
    where
    go [] = []
    go tokens = pre : breakBlocks post
        -- Start the indent off at 1, this way the next line in the 0th column
        -- will start a new block.
        where (pre, post) = breakBlock True 1 tokens
    -- Blank lines mess up the indentation.
    filterBlank [] = []
    filterBlank (Pos _ (Newline _) : xs@(Pos _ (Newline _) : _)) =
        filterBlank xs
    filterBlank (x:xs) = x : filterBlank xs

-- | Take until newline, then take lines until there the indent decreases.
breakBlock :: Bool -> Int -> [Token] -> ([Token], [Token])
breakBlock _ _ [] = ([], [])
breakBlock firstLine indent (t@(Pos _ (Newline lineIndent)) : ts)
    -- An indent less than the established block indent means I'm done.
    | lineIndent < indent = ([t], ts)
    | otherwise =
        let (pre, post) = breakBlock False blockIndent ts in (t:pre, post)
    where blockIndent = if firstLine then lineIndent else indent
breakBlock firstLine indent (t : ts) =
    let (pre, post) = breakBlock firstLine indent ts in (t:pre, post)


-- * extract tags

-- | Get all the tags in one indented block.
--
-- Search for:
-- - module <word>
-- - newtype <word>
-- - type <word>
-- - data <word> * = <word> :: or | <word>
-- - class * => <word> * where - strip indent, look for <word> ::
-- - <word> :: emit as a definition
-- - whitespace: skip to next line
blockTags :: [Token] -> [Tag]
blockTags tokens = case tokens of
    [] -> []
    Pos _ (Token "module") : Pos pos (Token name) : _ ->
        [mktag pos (snd (T.breakOnEnd "." name)) Module]
    -- newtype X * = X *
    Pos _ (Token "newtype") : Pos pos (Token name) : rest ->
        mktag pos name Type : newtypeTags pos rest
    -- type X * = ...
    Pos _ (Token "type") : Pos pos (Token name) : _ -> [mktag pos name Type]
    -- data X * = X { X :: *, X :: * }
    -- data X * where ...
    Pos _ (Token "data") : Pos pos (Token name) : rest ->
        mktag pos name Type : dataTags pos rest
    -- class * => X where X :: * ...
    Pos pos (Token "class") : rest -> classTags pos rest
    Pos pos (Token name) : Pos _ (Token "::") : _ -> [mktag pos name Function]
    _ -> []

mktag :: SrcPos -> Text -> Type -> Pos TagVal
mktag pos name typ = Pos pos (Tag name typ)

warning :: SrcPos -> String -> Pos TagVal
warning pos warn = Pos pos (Warning warn)

unexpected :: SrcPos -> [Token] -> [Token] -> String -> [Tag]
unexpected prevPos tokens tokensHere declaration =
    [warning pos ("unexpected " ++ thing ++ " after " ++ declaration)]
    where
    thing = if null tokensHere then "end of block"
        else show (valOf (head tokensHere))
    pos
        | not (null tokensHere) = posOf (head tokensHere)
        | not (null tokens) = posOf (last tokens)
        | otherwise = prevPos

-- | * = X *
newtypeTags :: SrcPos -> [Token] -> [Tag]
newtypeTags prevPos tokens = case dropUntil "=" tokens of
    Pos pos (Token name) : _ -> [mktag pos name Constructor]
    rest -> unexpected prevPos tokens rest "newtype * ="

-- | [] (empty data declaration)
-- * = X { X :: *, X :: * }
-- * where X :: * X :: *
-- * = X | X
dataTags :: SrcPos -> [Token] -> [Tag]
dataTags _ [] = [] -- empty data declaration
dataTags prevPos tokens
    -- if is gadt then...
    | otherwise = case dropUntil "=" tokens of
        Pos pos (Token name) : rest ->
            mktag pos name Constructor : collectRest rest
        rest -> unexpected prevPos tokens rest "data * ="
    where
    collectRest tokens = case tokens of
        Pos pos (Token name) : Pos _ (Token "::") : rest ->
            mktag pos name Function : collectRest rest
        Pos _ (Token "|") : Pos pos (Token name) : rest ->
            mktag pos name Constructor : collectRest rest
        _ : rest -> collectRest rest
        [] -> []

-- | * => X where X :: * ...
classTags :: SrcPos -> [Token] -> [Tag]
classTags prevPos unstripped = case tokens of
    Pos pos (Token name) : rest ->
        mktag pos name Class : collectRest rest
    rest -> unexpected prevPos tokens rest "class * =>"
    where
    tokens = if any ((== Token "=>") . valOf) unstripped
        then dropUntil "=>" unstripped else unstripped
    collectRest tokens = case tokens of
        Pos pos (Token name) : Pos _ (Token "::") : rest ->
            mktag pos name Function : collectRest rest
        _ : rest -> collectRest rest
        [] -> []

dropLine :: [Token] -> [Token]
dropLine = drop 1 . dropWhile (not . isNewline)

isNewline :: Token -> Bool
isNewline (Pos _ (Newline _)) = True
isNewline _ = False

dropUntil :: Text -> [Token] -> [Token]
dropUntil token = drop 1 . dropWhile ((/= Token token) . valOf)


-- * misc

(<>) :: (Monoid.Monoid a) => a -> a -> a
(<>) = Monoid.mappend

-- | If @op@ raised ENOENT, return Nothing.
catchENOENT :: IO a -> IO (Maybe a)
catchENOENT op = Exception.handleJust (guard . IO.Error.isDoesNotExistError)
    (const (return Nothing)) (fmap Just op)

dropDups :: (Eq k) => (a -> k) -> [a] -> [a]
dropDups _ [] = []
dropDups key (x:xs) = x : map snd (filter (not . equal) (zip (x:xs) xs))
    where equal (x, y) = key x == key y
