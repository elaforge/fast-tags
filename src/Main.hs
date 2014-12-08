{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

{- | Tagify haskell source.

    Annotate lines, strip comments, tokenize, then search for
    It loads the existing tags, and updates it for the given file.  Then
    a SaveBuf action runs tags every time you save a file.
    Or extend lushtags, the question is complete parsing ok?  It means I can't
    do it on save, since I'd have to invoke the build system to de-hsc.
-}

module Main
  ( main
  , TokenVal(..)
  , TagVal(..)
  , Type(..)
  , Tag(..)
  , Pos(..)
  , UnstrippedTokens(..)
  , breakString
  , stripComments
  , processAll
  , process
  , tokenize
  , stripCpp
  , annotate
  , stripNewlines
  , breakBlocks
  , unstrippedTokensOf
  )
where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.IntSet (IntSet)
import Data.Function (on)
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import System.Console.GetOpt
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.Exit
import System.FilePath ((</>))

import Language.Preprocessor.Unlit
import Text.Printf (printf)

import qualified Control.Exception as Exception
import qualified Data.Char as Char
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Environment as Environment
import qualified System.IO as IO

main :: IO ()
main = do
    args <- Environment.getArgs

    (flags, inputFiles) <- case getOpt Permute options args of
        (flags, inputs, []) -> return (flags, inputs)
        (_, _, errs)        -> let errMsg = "flag errors:\n" ++
                                            L.intercalate ", " errs
                               in usage $ errMsg ++ "\n" ++ help

    when (Help `elem` flags) $ usage help

    let verbose       = Verbose `elem` flags
        emacs         = ETags `elem` flags
        vim           = not emacs
        recurse       = Recurse `elem` flags
        output        = last $ defaultOutput : [fn | Output fn <- flags]
        noMerge       = NoMerge `elem` flags
        useZeroSep    = ZeroSep `elem` flags
        ignoreEncErrs = IgnoreEncodingErrors `elem` flags
        sep           = if useZeroSep then '\0' else '\n'
        defaultOutput = if vim then "tags" else "TAGS"
        inputsM       = if null inputFiles
                        then split sep <$> getContents
                        else fmap concat $ forM inputFiles $ \input -> do
                            -- if an input is a directory then we find the
                            -- haskell files inside it, optionally recursing
                            -- further if the -R switch is specified
                            isDirectory <- doesDirectoryExist input
                            if isDirectory
                                then filter isHsFile <$> contents recurse input
                                else return [input]

    oldTags <-
      if vim && not noMerge
         then do
              exists <- doesFileExist output
              if exists
                then T.lines <$> T.readFile output
                else return [vimMagicLine]
         else return [] -- we do not support tags merging for emacs
                        -- for now

    inputs <- inputsM
    -- This will merge and sort the new tags.  But I don't run it on the
    -- the result of merging the old and new tags, so tags from another
    -- file won't be sorted properly.  To do that I'd have to parse all the
    -- old tags and run processAll on all of them, which is a hassle.
    -- TODO try it and see if it really hurts performance that much.
    newTags <- fmap processAll $
      forM (zip [0..] inputs) $ \(i :: Int, fn) -> do
        (newTags, warnings) <- processFile ignoreEncErrs fn
        forM_ warnings printErr
        when verbose $ do
          let line = take 78 $ show i ++ ": " ++ fn
          putStr $ '\r' : line ++ replicate (78 - length line) ' '
          IO.hFlush IO.stdout
        return newTags

    when verbose $ putChar '\n'

    let write =
          if output == "-"
            then T.hPutStr IO.stdout
            else T.writeFile output

    write $
      if vim
        then T.unlines $ mergeTags inputs oldTags newTags
        else T.concat $ prepareEmacsTags newTags

  where
    usage msg = putStr (usageInfo msg options) >> exitSuccess

    contents recurse = if recurse
                         then getRecursiveDirContents
                         else getProperDirContents

-- | Crude predicate for Haskell files
isHsFile :: FilePath -> Bool
isHsFile fn = ".hs" `L.isSuffixOf` fn || isLiterateFile fn

isLiterateFile :: FilePath -> Bool
isLiterateFile fn = ".lhs" `L.isSuffixOf` fn

-- | Get all absolute filepaths contained in the supplied topdir,
-- except "." and ".."
getProperDirContents :: FilePath -> IO [FilePath]
getProperDirContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  return $ map ((</>) topdir) properNames

-- | Recurse directories collecting all files
getRecursiveDirContents :: FilePath -> IO [FilePath]
getRecursiveDirContents topdir = do
  paths  <- getProperDirContents topdir
  paths' <- forM paths $ \path -> do
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveDirContents path
      else return [path]
  return (concat paths')


type TagsTable = Map FilePath [Pos TagVal]

prepareEmacsTags :: [Pos TagVal] -> [Text]
prepareEmacsTags = printTagsTable . classifyTagsByFile

printTagsTable :: TagsTable -> [Text]
printTagsTable = map (uncurry printSection) . M.assocs

printSection :: FilePath -> [Pos TagVal] -> Text
printSection file tags =
  T.concat ["\x0c\x0a", T.pack file, ",",
            T.pack $ show tagsLength, "\x0a", tagsText]
  where
    tagsText = T.unlines $ map printEmacsTag tags
    tagsLength = T.length tagsText

printEmacsTag :: Pos TagVal -> Text
printEmacsTag (Pos (SrcPos _file line) (TagVal prefix _text _type)) =
    T.concat [prefix, "\x7f", T.pack (show line)]

classifyTagsByFile :: [Pos TagVal] -> TagsTable
classifyTagsByFile = foldr insertTag M.empty

insertTag :: Pos TagVal -> TagsTable -> TagsTable
insertTag tag@(Pos (SrcPos file _) _) table =
  M.insertWith (<>) file [tag] table

mergeTags :: [FilePath] -> [Text] -> [Pos TagVal] -> [Text]
mergeTags inputs old new =
    -- 'new' was already been sorted by 'process', but then I just concat
    -- the tags from each file, so they need sorting again.
    merge (map showTag new) (filter (not . isNewTag textFns) old)
    where textFns = S.fromList $ map T.pack inputs

data Flag = Output FilePath
          | Help
          | Verbose
          | ETags
          | Recurse
          | NoMerge
          | ZeroSep
          | IgnoreEncodingErrors
    deriving (Eq, Show)

help :: String
help = "usage: fast-tags [options] [filenames]\n" ++
       "In case no filenames provided on commandline, fast-tags expects " ++
       "list of files separated by newlines in stdin."

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"] (NoArg Help)
        "print help message"
    , Option ['o'] [] (ReqArg Output "file")
        "output file, defaults to 'tags'"
    , Option ['e'] [] (NoArg ETags)
        "print tags in Emacs format"
    , Option ['v'] [] (NoArg Verbose)
        "print files as they are tagged, useful to track down slow files"
    , Option ['R'] [] (NoArg Recurse)
        "read all files under any specified directories recursively"
    , Option ['0'] [] (NoArg ZeroSep)
        "expect list of file names in stdin to be 0-separated."
    , Option [] ["nomerge"] (NoArg NoMerge)
        "do not merge tag files"
    , Option [] ["ignore-encoding-errors"] (NoArg IgnoreEncodingErrors)
        "do exit on utf8 encoding error and continue processing other files"
    ]

-- | Documented in vim :h tags-file-format.
-- This tells vim that the file is sorted (but not case folded) so that
-- it can do a bsearch and never needs to fall back to linear search.
vimMagicLine :: Text
vimMagicLine = "!_TAG_FILE_SORTED\t1\t~"

isNewTag :: Set Text -> Text -> Bool
isNewTag textFns line = S.member fn textFns
    where fn = T.takeWhile (/='\t') $ T.drop 1 $ T.dropWhile (/='\t') line

merge :: (Ord a) => [a] -> [a] -> [a]
merge = mergeBy compare

mergeOn :: (Ord b) => (a -> b) -> [a] -> [a] -> [a]
mergeOn f = mergeBy (compare `on` f)

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f xs ys = go xs ys
  where
    go []     ys     = ys
    go xs     []     = xs
    go (x:xs) (y:ys) =
      case f x y of
        EQ -> x: y: go xs ys
        LT -> x: go xs (y:ys)
        GT -> y: go (x:xs) ys

-- | Convert a Tag to text, e.g.: AbsoluteMark\tCmd/TimeStep.hs 67 ;" f
showTag :: Pos TagVal -> Text
showTag (Pos (SrcPos fn lineno) (TagVal _ text typ)) =
    T.concat [text, "\t", T.pack fn, "\t", T.pack (show lineno), " ;\" ",
        T.singleton (showType typ)]

-- | Vim takes this to be the \"kind:\" annotation.  It's just an arbitrary
-- string and these letters conform to no standard.  Presumably there are some
-- vim extensions that can make use of it.
showType :: Type -> Char
showType typ = case typ of
    Module      -> 'm'
    Function    -> 'f'
    Class       -> 'c'
    Type        -> 't'
    Constructor -> 'C'
    Operator    -> 'o'
    Pattern     -> 'p'

-- * types

data TagVal = TagVal !Text !Text !Type
    deriving (Show)

instance Eq TagVal where
  TagVal _ name t == TagVal _ name' t' = name == name' && t == t'

instance Ord TagVal where
  compare (TagVal _ name t) (TagVal _ name' t') =
    name `compare` name' <> t `compare` t'

-- don't swap constructors since we rely that Type < Constructor == True holds
data Type =
    Function
  | Type
  | Constructor
  | Class
  | Module
  | Operator
  | Pattern
  deriving (Eq, Ord, Show)

data TokenVal =
    Token !Text !Text
  | Newline !Int -- ^ indentation
  deriving (Show)

tokenName :: TokenVal -> Text
tokenName (Token _ name) = name
tokenName _              = error "cannot extract name from non-Token TokenVal"

-- tokenNameSafe :: TokenVal -> Text
-- tokenNameSafe (Token _ name) = name
-- tokenNameSafe _              = "<newline>"

data Tag =
    Tag !(Pos TagVal)
  | RepeatableTag !(Pos TagVal)
  | Warning !String
  deriving (Show, Eq, Ord)

partitionTags :: [Tag] -> ([Pos TagVal], [Pos TagVal], [String])
partitionTags ts = go ts [] [] []
  where
    go []                    xs ys zs = (xs, ys, reverse zs)
    go (Tag t: ts)           xs ys zs = go ts (t:xs) ys     zs
    go (RepeatableTag t: ts) xs ys zs = go ts xs     (t:ys) zs
    go (Warning warn: ts)    xs ys zs = go ts xs     ys     (warn :zs)

type Token = Pos TokenVal

-- | Newlines have to remain in the tokens because 'breakBlocks' relies on
-- them.  But they make pattern matching on the tokens unreliable because
-- newlines might be anywhere.  A newtype makes sure that the tokens only get
-- stripped once and that I don't do any pattern matching on unstripped tokens.
newtype UnstrippedTokens = UnstrippedTokens [Token]
    deriving (Show, Monoid)

mapTokens :: ([Token] -> [Token]) -> UnstrippedTokens -> UnstrippedTokens
mapTokens f (UnstrippedTokens tokens) = UnstrippedTokens (f tokens)

unstrippedTokensOf :: UnstrippedTokens -> [Token]
unstrippedTokensOf (UnstrippedTokens tokens) = tokens

-- drop @n@ non-newline tokens
dropTokens :: Int -> UnstrippedTokens -> UnstrippedTokens
dropTokens n = mapTokens (f n)
  where
    f :: Int -> [Token] -> [Token]
    f 0 xs                       = xs
    f _ []                       = []
    f n (Pos _ (Newline _) : xs) = f n xs
    f n (Pos _ (Token _ _) : xs) = f (n - 1) xs

type Line = Pos Text

data Pos a = Pos
    { posOf :: !SrcPos
    , valOf :: !a
    }
    deriving (Eq, Ord)

data SrcPos = SrcPos
    { _posFile :: !FilePath
    , posLine  :: !Int
    } deriving (Eq, Ord)

instance (Show a) => Show (Pos a) where
    show (Pos pos val) = show pos ++ ":" ++ show val
instance Show SrcPos where
    show (SrcPos fn line) = fn ++ ":" ++ show line

-- * process

-- | Global processing for when all tags are together.
processAll :: [[Pos TagVal]] -> [Pos TagVal]
processAll =
  sortDups .
  dropDups isDuplicatePair .
  combineBalanced (mergeOn tagSortingKey) .
  map (dropDups isDuplicatePair)
  where
    isDuplicatePair :: Pos TagVal -> Pos TagVal -> Bool
    isDuplicatePair t t' =
      f == f' &&
      (l == l' ||
       -- special hack to handle the case when type is on a separate line and
       -- constructor is on the next, e.g.
       -- data Foo a =,
       --   Foo a Int
       --   deriving (Show, Eq, Ord),
       (l + 1) == l') &&
      tagText t == tagText t'
      where
        SrcPos f  l  = posOf t
        SrcPos f' l' = posOf t'

combineBalanced :: forall a. (a -> a -> a) -> [a] -> a
combineBalanced f xs = go xs
  where
    go :: [a] -> a
    go [] = error "cannot combine empty list"
    go xs@(_:_) =
      case combine xs of
        []  -> error "unexpected empty list when combining nonempty lists"
        [x] -> x
        xs' -> go xs'
    combine :: [a] -> [a]
    combine []        = []
    combine [x]       = [x]
    combine (x:x':xs) = f x x' : combine xs

-- | Given multiple matches, vim will jump to the first one.  So sort adjacent
-- tags with the same text by their type.
--
-- Mostly this is so that given a type with the same name as its module,
-- the type will come first.
sortDups :: [Pos TagVal] -> [Pos TagVal]
sortDups =
  concatMap (sortOn tagType) .
  M.elems .
  M.fromAscListWith (++) .
  map (fst . tagSortingKey &&& (:[]))

tagText :: Pos TagVal -> Text
tagText (Pos _ (TagVal _ text _)) = text

tagType :: Pos TagVal -> Type
tagType (Pos _ (TagVal _ _ t)) = t

tagLine :: Pos TagVal -> Int
tagLine (Pos (SrcPos _ line) _) = line

-- | Read tags from one file.
processFile :: Bool -> FilePath -> IO ([Pos TagVal], [String])
processFile ignoreEncodingErrors fn = fmap (process fn) (T.readFile fn)
  `Exception.catch` \(exc :: Exception.SomeException) -> do
    -- readFile will crash on files that are not UTF8.  Unfortunately not
    -- all haskell source file are.
    IO.hPutStrLn IO.stderr $
      "exception reading " ++ show fn ++ ": " ++ show exc
    unless ignoreEncodingErrors $
      void $ exitFailure
    return ([], [])

tagSortingKey :: Pos TagVal -> (Text, Type)
tagSortingKey (Pos _ (TagVal _ name t)) = (name, t)

-- | Process one file's worth of tags.
process :: FilePath -> Text -> ([Pos TagVal], [String])
process fn = splitAndRemoveRepeats . concatMap blockTags . breakBlocks . stripComments
  . mconcat . map tokenize . stripCpp . annotate fn . unlit'
  where
    splitAndRemoveRepeats :: [Tag] -> ([Pos TagVal], [String])
    splitAndRemoveRepeats tags =
      (mergeOn tagSortingKey (sortOn tagSortingKey newTags) earliestRepeats, warnings)
      where
        (newTags, repeatableTags, warnings) = partitionTags tags
        earliestRepeats :: [Pos TagVal]
        earliestRepeats =
          M.elems $
          M.fromListWith minLine $
          map (tagSortingKey &&& id) repeatableTags
        minLine x y
          | tagLine x < tagLine y = x
          | otherwise             = y
    unlit' :: Text -> Text
    unlit' s = if isLiterateFile fn
               then T.pack $ unlit fn $ T.unpack s'
               else s
      where
        s' :: Text
        s' = if "\\begin{code}" `T.isInfixOf` s &&
                "\\end{code}"   `T.isInfixOf` s
             then T.unlines $ filter (not . birdLiterateLine) $ T.lines s
             else s
        birdLiterateLine :: Text -> Bool
        birdLiterateLine xs
          | T.null xs = False
          | otherwise =
            case T.uncons $ T.dropWhile Char.isSpace xs of
              Just ('>', _) -> True
              _             -> False

-- * tokenize

annotate :: FilePath -> Text -> [Line]
annotate fn text =
    [Pos (SrcPos fn num) line | (num, line) <- zip [1..] (T.lines text)]

-- | Also strips out hsc detritus.
stripCpp :: [Line] -> [Line]
stripCpp = filter $ not . ("#" `T.isPrefixOf`) . valOf

tokenize :: Line -> UnstrippedTokens
tokenize (Pos pos line) = UnstrippedTokens $ map (Pos pos) (tokenizeLine line)

spanToken :: Text -> (Text, Text)
spanToken text
    -- ☡ special case to fight "--" biting too much input so that closing "-}"
    -- becomes unavailable
    | "--}" `T.isPrefixOf` text
    = ("-}", T.drop 2 cs) -- T.drop 3 text)
    | Just sym <- L.find (`T.isPrefixOf` text) comments
    = (sym, T.tail cs) -- T.drop 2 text)
    | Just sym <- L.find (`T.isPrefixOf` text) symbols
    , let t = T.tail cs -- T.drop 2 text
    , T.null t || not (haskellOpChar $ T.head t)
    = (sym, t)
    | c == '\''
    = let (token, rest) = breakChar   cs in (T.cons c token, rest)
    | c == '"'
    = let (token, rest) = breakString cs in (T.cons c token, rest)
    | state@(token, _) <- spanSymbol (haskellOpChar c) text,
      not (T.null token)
    = state
    | otherwise
    -- This will tokenize differently than haskell should, e.g., 9x will
    -- be "9x" not "9" "x".  But I just need a wordlike chunk, not an
    -- actual token.  Otherwise I'd have to tokenize numbers.
    = case T.span (identChar $ Char.isUpper c) text of
        ("", _)       -> (T.singleton c, cs)
        (token, rest) -> (token, rest)
  where
    Just (c, cs) = T.uncons text
    comments = ["{-", "-}"]
    symbols = ["--", "=>", "->", "::"]

tokenizeLine :: Text -> [TokenVal]
tokenizeLine text = Newline nspaces : go spaces line
  where
    nspaces = fromIntegral $ T.count " " spaces + T.count "\t" spaces * 8
    (spaces, line) = T.break (not . Char.isSpace) text
    go :: Text -> Text -> [TokenVal]
    go oldPrefix unstripped
      | T.null stripped = []
      | otherwise       = let (token, rest) = spanToken stripped
                              newPrefix     = oldPrefix <> spaces <> token
                          in Token newPrefix token : go newPrefix rest
      where
        (spaces, stripped) = T.break (not . Char.isSpace) unstripped

startIdentChar :: Char -> Bool
startIdentChar c = Char.isAlpha c || c == '_'

identChar :: Bool -> Char -> Bool
identChar considerDot c = Char.isAlphaNum c ||
                          c == '\'' ||
                          c == '_' ||
                          c == '#' ||
                          considerDot && c == '.'

-- unicode operators are not supported yet
haskellOpChar :: Char -> Bool
haskellOpChar = \c -> IS.member (Char.ord c) opChars
  where
    opChars :: IntSet
    opChars = IS.fromList $ map Char.ord "-!#$%&*+./<=>?@^|~:\\"

isTypeVarStart :: Text -> Bool
isTypeVarStart x =
  case T.uncons x of
    Just (c, _) -> Char.isLower c || c == '_'
    _           -> False

-- | Span a symbol, making sure to not eat comments.
spanSymbol :: Bool -> Text -> (Text, Text)
spanSymbol considerColon text
  | Just res <- haskellOp text [] =
    res
  | any (`T.isPrefixOf` post) [",", "--", "-}", "{-"] =
    split
  | Just (c, cs) <- T.uncons post
  , c == '-' || c == '{' =
    let (pre2, post2) = spanSymbol considerColon cs
    in (pre <> T.cons c pre2, post2)
  | otherwise = split
  where
    split@(pre, post) = T.break (\c -> T.any (==c) "-{," || not (symbolChar considerColon c)) text

    haskellOp :: Text -> [Char] -> Maybe (Text, Text)
    haskellOp txt op | Just (c, cs) <- T.uncons txt
                     , haskellOpChar c
                     , not $ "-}" `T.isPrefixOf` txt =
                       haskellOp cs $ c: op
                     | null op   = Nothing
                     | otherwise = Just (T.pack $ reverse op, txt)



symbolChar :: Bool -> Char -> Bool
symbolChar considerColon c = (Char.isSymbol c || Char.isPunctuation c) &&
                             (not (c `elem` "(),;[]`{}_:\"'") ||
                              considerColon && c == ':')

breakChar :: Text -> (Text, Text)
breakChar text
  | T.null text = ("", "")
  | T.head text == '\\' = T.splitAt 3 text
  | otherwise = T.splitAt 2 text

-- TODO \ continuation isn't supported.  I'd have to tokenize at the file
-- level instead of the line level.
breakString :: Text -> (Text, Text)
breakString = (T.pack . reverse *** T.pack) . go [] . T.unpack
  where
    go :: String -> String -> (String, String)
    go s []             = (s, [])
    go s ('"':xs)       = ('"': s, xs)
    go s ('\\':[])      = (s, "\\")
    -- handle string continuation
    go s ('\\':'\n':xs) = go s $ dropBackslash $ dropWhile (\c -> c /= '\\' && c /= '"') xs
    go s ('\\':x:xs)    = go (x: '\\': s) xs
    go s (x:xs)         = go (x: s) xs

    dropBackslash :: String -> String
    dropBackslash ('\\':xs) = xs
    dropBackslash xs        = xs

stripComments :: UnstrippedTokens -> UnstrippedTokens
stripComments = mapTokens (go 0)
  where
    go :: Int -> [Token] -> [Token]
    go _ [] = []
    go nest (pos@(Pos _ token) : rest)
      | token `nameStartsWith` "{-"                     = go (nest + 1) rest
      | token `nameEndsWith` "-}"                       = go (nest - 1) rest
      | nest == 0 && tokenNameSatisfies token isComment = go nest (dropLine rest)
      | nest > 0                                        = go nest rest
      | otherwise                                       = pos: go nest rest
    dropLine :: [Token] -> [Token]
    dropLine = dropWhile (not . isNewline)
    isComment :: Text -> Bool
    isComment name =
      "--" `T.isPrefixOf` name &&
      T.all (\c -> not (haskellOpChar c) || c == '-') (T.drop 2 name)

-- | Break the input up into blocks based on indentation.
breakBlocks :: UnstrippedTokens -> [UnstrippedTokens]
breakBlocks = map UnstrippedTokens . filter (not . null) .
              go . filterBlank . unstrippedTokensOf
  where
    go :: [Token] -> [[Token]]
    go []     = []
    go tokens = pre : go post
      where
        (pre, post) = breakBlock tokens
    -- Blank lines mess up the indentation.
    filterBlank :: [Token] -> [Token]
    filterBlank []                                             = []
    filterBlank (Pos _ (Newline _): xs@(Pos _ (Newline _): _)) = filterBlank xs
    filterBlank (x:xs)                                         = x: filterBlank xs

-- | Take until a newline, then take lines until the indent established after
-- that newline decreases. Or, alternatively, if "{" is encountered then count
-- it as a block until closing "}" is found taking nesting into account.
breakBlock :: [Token] -> ([Token], [Token])
breakBlock (t@(Pos _ tok):ts) = case tok of
  Newline indent -> collectIndented indent ts
  Token _ "{"    -> collectBracedBlock breakBlock ts 1
  _              -> remember t $ breakBlock ts
  where
    collectIndented :: Int -> [Token] -> ([Token], [Token])
    collectIndented indent tsFull@(t@(Pos _ tok): ts) =
      case tok of
        Newline n | n <= indent -> ([], tsFull)
        Token _ "{" -> remember t $ collectBracedBlock (collectIndented indent) ts 1
        _           -> remember t $ collectIndented indent ts
    collectIndented _ [] = ([], [])

    collectBracedBlock :: ([Token] -> ([Token], [Token])) ->
                          [Token] ->
                          Int ->
                          ([Token], [Token])
    collectBracedBlock _    []                           _ = ([], [])
    collectBracedBlock cont ts                           0 = cont ts
    collectBracedBlock cont (t@(Pos _ (Token _ "{")):ts) n =
      remember t $ collectBracedBlock cont ts $! n + 1
    collectBracedBlock cont (t@(Pos _ (Token _ "}")):ts) n =
      remember t $ collectBracedBlock cont ts $! n - 1
    collectBracedBlock cont (t:ts)                       n =
      remember t $ collectBracedBlock cont ts n

    remember :: Token -> ([Token], [Token]) -> ([Token], [Token])
    remember t (xs, ys) = (t: xs, ys)
breakBlock [] = ([], [])

-- * extract tags

-- | Get all the tags in one indented block.
blockTags :: UnstrippedTokens -> [Tag]
blockTags unstripped = case stripNewlines unstripped of
    [] -> []
    Pos _ (Token _ "module"): Pos pos (Token prefix name): _ ->
        [mkTag pos prefix (snd (T.breakOnEnd "." name)) Module]
    Pos _ (Token _ "pattern"): Pos pos (Token prefix name): _
      | not (T.null name) && Char.isUpper (T.head name) ->
        [mkTag pos prefix name Pattern]
    Pos _ (Token _ "foreign"): decl -> foreignTags decl
    -- newtype instance * = ...
    Pos _ (Token _ "newtype"): Pos _ (Token _ "instance"): (dropDataContext -> Pos pos _: rest) ->
       newtypeTags pos rest
    -- newtype X * = X *
    Pos _ (Token _ "newtype"): (dropDataContext -> whole@(tok@(Pos pos (Token _ name)): rest)) ->
      if isTypeName name
      then tokToTag tok Type : newtypeTags pos rest
      else let (pos', tok, rest') = recordInfixName Type whole
           in tok: newtypeTags pos' rest'
    -- type family X ...
    Pos _ (Token _ "type"): Pos _ (Token _ "family"): (dropDataContext -> whole@(tok@(Pos _ (Token _ name)): _)) ->
      if isTypeFamilyName name
      then [tokToTag tok Type]
      else let (_, tok, _) = recordInfixName Type whole
           in [tok]
    -- type X * = ...
    Pos _ (Token _ "type"): (dropDataContext -> whole@(tok@(Pos _ (Token _ name)): _)) ->
      if isTypeName name
      then [tokToTag tok Type]
      else let (_, tok, _) = recordInfixName Type whole
           in [tok]
    -- data family X ...
    Pos _ (Token _ "data"): Pos _ (Token _ "family"): (dropDataContext -> tok@(Pos _ (Token _ name)): rest) ->
      if isTypeFamilyName name
      then [tokToTag tok Type]
      else let (_, tok, _) = recordInfixName Type rest
           in [tok]
    -- data instance * = ...
    -- data instance * where ...
    Pos _ (Token _ "data"): Pos _ (Token _ "instance"): (dropDataContext -> Pos pos _: _) ->
      dataConstructorTags pos (dropTokens 2 unstripped)
    -- data X * = X { X :: *, X :: * }
    -- data X * where ...
    Pos _ (Token _ "data"): (dropDataContext -> tok@(Pos pos (Token _ name)): rest) ->
      if isTypeName name
      then tokToTag tok Type : dataConstructorTags pos (dropTokens 2 unstripped)
      -- if token after data is not a type name then it isn't
      -- infix type as well since it may be only '(' or some
      -- lowercase name, either of which is not type constructor
      else let (pos', tok, _) = recordInfixName Type rest
           in tok: dataConstructorTags pos' (dropTokens 1 unstripped)
    -- class * => X where X :: * ...
    Pos pos (Token _ "class") : _ -> classTags pos (dropTokens 1 unstripped)

    Pos _ (Token _ "infix") : _ -> []
    Pos _ (Token _ "infixl") : _ -> []
    Pos _ (Token _ "infixr") : _ -> []
    -- instance * where data * = X :: * ...
    Pos pos (Token _ "instance") : _ -> instanceTags pos (dropTokens 1 unstripped)
    -- x, y, z :: *
    stripped -> toplevelFunctionTags stripped

isTypeFamilyName :: Text -> Bool
isTypeFamilyName x = not (T.null x) && (Char.isUpper c || haskellOpChar c) where c = T.head x

isTypeName  :: Text -> Bool
isTypeName x =
  case T.uncons x of
    Just (c, _) -> Char.isUpper c || c == ':'
    _           -> False

dropDataContext :: [Token] -> [Token]
dropDataContext = stripParensKindsTypeVars . stripOptContext

recordInfixName :: Type -> [Token] -> (SrcPos, Tag, [Token])
recordInfixName tokenType tokens = (pos, tokToTag tok tokenType, rest)
  where
    (tok@(Pos pos _) : rest) = dropInfixTypeStart tokens

-- same as dropWhile with counting
dropInfixTypeStart :: [Token] -> [Token]
dropInfixTypeStart tokens = dropWhile f tokens
  where
    f (Pos _ (Token _ name)) = isInfixTypePrefix name ||
                               (T.length name == 1 && T.head name == '`')
    f _                      = False

    isInfixTypePrefix :: Text -> Bool
    isInfixTypePrefix x = Char.isLower c || c == '(' where c = T.head x


-- | It's easier to scan for tokens without pesky newlines popping up
-- everywhere.  But I need to keep the newlines in in case I hit a @where@
-- and need to call 'breakBlocks' again.
stripNewlines :: UnstrippedTokens -> [Token]
stripNewlines = filter (not . isNewline) . (\(UnstrippedTokens t) -> t)

-- | introduce tags for foreign imports
foreignTags :: [Token] -> [Tag]
foreignTags = \decl ->
  case decl of
    Pos _ (Token _ "import"): decl' ->
      let name = last $
                 takeWhile ((/= "::") . tokenName . valOf) $
                 dropMember safety $
                 dropMember callConv decl'
      in [tokToTag name Pattern]
    _ -> []
  where
    dropMember :: Set Text -> [Token] -> [Token]
    dropMember dropNames ts@(Pos _ (Token _ name): rest)
      | S.member name dropNames = rest
      | otherwise               = ts
    dropMember _ ts = ts

    safety :: Set Text
    safety = S.fromList ["safe", "unsafe"]

    callConv :: Set Text
    callConv = S.fromList ["ccall", "stdcall", "cplusplus", "jvm", "dotnet"]

toplevelFunctionTags :: [Token] -> [Tag]
toplevelFunctionTags toks =
  case tags of
    -- tags of toplevel functions are all repeatable, even the ones that come from
    -- the type signature because there will definitile be tags from the body and they
    -- should be sorted out if type signature is present.
    [] -> functionTagsNoSig toks
    _  -> map toRepeatableTag $ tags
  where
    -- first try to detect tags from type signature, if it fails then
    -- do the actual work of detecting from body
    (tags, _) = functionTags False toks
    toRepeatableTag :: Tag -> Tag
    toRepeatableTag (Tag t) = RepeatableTag t
    toRepeatableTag t       = t

functionTagsNoSig :: [Token] -> [Tag]
functionTagsNoSig toks = go toks
  where
    go :: [Token] -> [Tag]
    go []                           = []
    go toks@(Pos _ (Token _ "("):_) = go $ stripBalancedParens toks
    go (Pos pos (Token prefix name):ts)
      | name == "::"               = [] -- this function does not analyze type signatures
      | name == "!" || name == "~" = go ts
      | name == "=" || name == "|" =
        case stripParens toks of
          Pos pos (Token prefix name): _
            | functionName False name  -> [mkRepeatableTag pos prefix name Function]
            | T.all haskellOpChar name -> [mkRepeatableTag pos prefix name Operator]
          _ -> []
      | name == "`"                =
        case ts of
          Pos pos' (Token prefix' name'):_
            | functionName False name' -> [mkRepeatableTag pos' prefix' name' Function]
          _ -> go ts
      | T.all haskellOpChar name   = [mkRepeatableTag pos prefix name Operator]
      | otherwise                  = go ts
    stripParens :: [Token] -> [Token]
    stripParens = dropWhile ((`hasName` "(") . valOf)

-- | Get tags from a function type declaration: token , token , token ::
-- Return the tokens left over.
functionTags :: Bool -> -- ^ expect constructors, not functions
                [Token] ->
                ([Tag], [Token])
functionTags constructors = go []
  where
    opTag   = if constructors then Constructor else Operator
    funcTag = if constructors then Constructor else Function
    go :: [Tag] -> [Token] -> ([Tag], [Token])
    go tags (Pos _ (Token _ "("): Pos pos (Token _ name): Pos _ (Token prefix ")"): Pos _ (Token _ "::"): rest) =
        (reverse $ mkTag pos prefix name opTag: tags, rest)
    go tags (Pos pos (Token prefix name): Pos _ (Token _ "::"): rest)
        | functionName constructors name =
        (reverse $ mkTag pos prefix name funcTag : tags, rest)
    go tags (Pos _ (Token _ "("): Pos pos (Token _ name): Pos _ (Token prefix ")"): Pos _ (Token _ ","): rest) =
        go (mkTag pos prefix name opTag: tags) rest
    go tags (Pos pos (Token prefix name): Pos _ (Token _ ","): rest)
        | functionName constructors name =
        go (mkTag pos prefix name funcTag: tags) rest
    go tags tokens = (tags, tokens)

functionName :: Bool -> Text -> Bool
functionName constructors text = isFunction text
  where
    isFunction text =
      case T.uncons text of
        Just (c, cs) -> firstChar c && startIdentChar c && T.all (identChar True) cs
        Nothing      -> False
    firstChar = if constructors
                then Char.isUpper
                else \c -> Char.isLower c || c == '_'

-- | * = X *
newtypeTags :: SrcPos -> [Token] -> [Tag]
newtypeTags prevPos tokens =
  case dropUntil "=" tokens of
    Pos pos (Token prefix name) : rest ->
        let constructor = mkTag pos prefix name Constructor
        in  case rest of
            Pos _ (Token _ "{"): Pos funcPos (Token funcPrefix funcName): _ ->
                [constructor, mkTag funcPos funcPrefix funcName Function]
            _ ->
                [constructor]
    rest -> unexpected prevPos (UnstrippedTokens tokens) rest "newtype * ="

-- | [] (empty data declaration)
-- * = X { X :: *, X :: * }
-- * where X :: * X :: *
-- * = X | X
dataConstructorTags :: SrcPos -> UnstrippedTokens -> [Tag]
dataConstructorTags prevPos unstripped
    -- GADT
    | any ((`hasName` "where") . valOf) (unstrippedTokensOf unstripped) =
        concatMap gadtTags (whereBlock unstripped)
    -- plain ADT
    | otherwise =
      case stripOptBang $ stripOptContext $ stripOptForall $ dropUntil "=" $
           stripNewlines unstripped of
        [] -> [] -- empty data declaration
        rest | Just (Pos pos (Token prefix name), rest') <-
               extractInfixConstructor rest ->
          mkTag pos prefix name Constructor : collectRest rest'
        Pos pos (Token prefix name) : rest ->
          mkTag pos prefix name Constructor : collectRest rest
        rest -> unexpected prevPos unstripped rest "data * ="
  where
    collectRest :: [Token] -> [Tag]
    collectRest tokens
      | (tags@(_:_), rest) <- functionTags False tokens =
          tags ++ collectRest (dropUntilNextField rest)
    collectRest (Pos pipePos (Token _ "|") : rest)
      | Just (Pos pos (Token prefix name), rest'') <-
          extractInfixConstructor rest' =
            mkTag pos prefix name Constructor: collectRest rest''
      | Pos pos (Token prefix name): rest'' <- rest' =
          mkTag pos prefix name Constructor : collectRest (dropUntilNextCaseOrRecordStart rest'')
      | otherwise = error (printf "syntax error@%d: | not followed by tokens\n" (posLine pipePos))
      where
        rest' = stripOptBang $ stripOptContext $ stripOptForall rest
    collectRest (_ : rest) = collectRest rest
    collectRest [] = []

    stripOptBang :: [Token] -> [Token]
    stripOptBang ((Pos _ (Token _ "!")): rest) = rest
    stripOptBang ts                            = ts

    extractInfixConstructor :: [Token] -> Maybe (Token, [Token])
    extractInfixConstructor = extract . stripTypeParam
      where
        extract :: [Token] -> Maybe (Token, [Token])
        extract (tok@(Pos _ (Token _ name)): rest)
          | not (T.null name) && T.head name == ':'
          = Just (tok, stripTypeParam rest)
        extract (Pos _ (Token _ "`"): tok@(Pos _ _): Pos _ (Token _ "`"): rest) =
          Just (tok, stripTypeParam rest)
        extract _ = Nothing
        stripTypeParam :: [Token] -> [Token]
        stripTypeParam input@((Pos _ (Token _ "(")): _) = stripBalancedParens input
        stripTypeParam input@((Pos _ (Token _ "[")): _) = stripBalancedBrackets input
        stripTypeParam ts                               = tailSafe ts

    dropUntilNextCaseOrRecordStart :: [Token] -> [Token]
    dropUntilNextCaseOrRecordStart =
      dropWithStrippingBalanced (\x -> not $ x == "|" || x == "{")

    dropUntilNextField :: [Token] -> [Token]
    dropUntilNextField =
      dropWithStrippingBalanced (\x -> not $ x == "," || x == "}" || x == "|")

stripOptForall :: [Token] -> [Token]
stripOptForall (Pos _ (Token _ "forall"): rest) = dropUntil "." rest
stripOptForall xs                               = xs

stripParensKindsTypeVars :: [Token] -> [Token]
stripParensKindsTypeVars (Pos _ (Token _ "("): xs)  =
  stripParensKindsTypeVars xs
-- stripParensKindsTypeVars (Pos _ (Token _ ")"): xs)  =
--   stripParensKindsTypeVars xs
stripParensKindsTypeVars (Pos _ (Token _ "::"): xs) =
  stripParensKindsTypeVars $ tail $ dropWithStrippingBalanced (/= ")") xs
stripParensKindsTypeVars (Pos _ (Token _ name): xs)
  | isTypeVarStart name = stripParensKindsTypeVars xs
stripParensKindsTypeVars xs = xs

stripOptContext :: [Token] -> [Token]
stripOptContext (stripBalancedParens -> (Pos _ (Token _ "=>"): xs))   = xs
stripOptContext (stripSingleClassContext -> Pos _ (Token _ "=>"): xs) = xs
stripOptContext xs                                                    = xs

stripSingleClassContext :: [Token] -> [Token]
stripSingleClassContext (Pos _ (Token _ name): xs)
  | Char.isUpper $ T.head name = dropWithStrippingBalanced isTypeVarStart xs
stripSingleClassContext xs = xs

-- drop all tokens for which @pred@ returns True, also drop
-- any parenthesized expressions
dropWithStrippingBalanced :: (Text -> Bool) -> [Token] -> [Token]
dropWithStrippingBalanced pred input@(Pos _ (Token _ name): xs)
  | name == "(" = dropWithStrippingBalanced pred $ stripBalancedParens input
  | name == "[" = dropWithStrippingBalanced pred $ stripBalancedBrackets input
  | pred name   = dropWithStrippingBalanced pred xs
dropWithStrippingBalanced _ xs = xs

stripBalancedParens :: [Token] -> [Token]
stripBalancedParens = stripBalanced "(" ")"

stripBalancedBrackets :: [Token] -> [Token]
stripBalancedBrackets = stripBalanced "[" "]"

stripBalanced :: Text -> Text -> [Token] -> [Token]
stripBalanced open close (Pos _ (Token _ name): xs)
  | name == open = go 1 xs
  where
    go :: Int -> [Token] -> [Token]
    go 0 xs                          = xs
    go !n (Pos _ (Token _ name): xs) | name == open  = go (n + 1) xs
                                     | name == close = go (n - 1) xs
                                     | otherwise     = go n       xs
    go n (_: xs)                     = go n xs
    go _ []                          = []
stripBalanced _ _ xs = xs


gadtTags :: UnstrippedTokens -> [Tag]
gadtTags = fst . functionTags True . stripNewlines

-- | * => X where X :: * ...
classTags :: SrcPos -> UnstrippedTokens -> [Tag]
classTags prevPos unstripped =
    case dropDataContext $ stripNewlines unstripped of
        whole@(tok@(Pos _ (Token _ name)): _) ->
            -- Drop the where and start expecting functions.
            let cont = concatMap classBodyTags (whereBlock unstripped)
            in  if isTypeName name
                then tokToTag tok Class: cont
                else let (_, tok, _) = recordInfixName Class whole
                     in tok: concatMap classBodyTags (whereBlock unstripped)
        rest -> unexpected prevPos unstripped rest "class * =>"

classBodyTags :: UnstrippedTokens -> [Tag]
classBodyTags unstripped = case stripNewlines unstripped of
    Pos _ (Token _ typedata) : Pos pos (Token prefix name) : _
        | typedata `elem` ["type", "data"] -> [mkTag pos prefix name Type]
    tokens -> fst $ functionTags False tokens

-- | Skip to the where and split the indented block below it.
whereBlock :: UnstrippedTokens -> [UnstrippedTokens]
whereBlock = breakBlocks . mapTokens (dropUntil "where")

instanceTags :: SrcPos -> UnstrippedTokens -> [Tag]
instanceTags prevPos unstripped =
  -- instances can offer nothing but some fresh data constructors since
  -- the actual datatype is really declared in the class declaration
  concatMap (newtypeTags prevPos . unstrippedTokensOf)
            (filter isNewtypeDecl block)
  ++
  concatMap (dataConstructorTags prevPos)
            (filter isDataDecl block)
  where
    block = whereBlock unstripped

    isNewtypeDecl :: UnstrippedTokens -> Bool
    isNewtypeDecl (UnstrippedTokens (Pos _ (Token _ "newtype"): _)) = True
    isNewtypeDecl _                                                 = False

    isDataDecl :: UnstrippedTokens -> Bool
    isDataDecl (UnstrippedTokens (Pos _ (Token _ "data"): _)) = True
    isDataDecl _                                              = False

-- * util

mkTag :: SrcPos -> Text -> Text -> Type -> Tag
mkTag pos prefix name typ = Tag $ Pos pos (TagVal prefix name typ)

mkRepeatableTag :: SrcPos -> Text -> Text -> Type -> Tag
mkRepeatableTag pos prefix name typ = RepeatableTag $ Pos pos (TagVal prefix name typ)

tokToTag :: Token -> Type -> Tag
tokToTag (Pos pos (Token prefix name)) t = mkTag pos prefix name t

warning :: SrcPos -> String -> Tag
warning pos warn = Warning $ show pos ++ ": " ++ warn

unexpected :: SrcPos -> UnstrippedTokens -> [Token] -> String -> [Tag]
unexpected prevPos (UnstrippedTokens tokensBefore) tokensHere declaration =
    [warning pos ("unexpected " ++ thing ++ " after " ++ declaration)]
    where
    thing = if null tokensHere
            then "end of block"
            else show (valOf (head tokensHere))
    pos
        | not (null tokensHere) = posOf (head tokensHere)
        | not (null tokensBefore) = posOf (last tokensBefore)
        | otherwise = prevPos

isNewline :: Token -> Bool
isNewline (Pos _ (Newline _)) = True
isNewline _ = False

hasName :: TokenVal -> Text -> Bool
hasName tok text = tokenNameSatisfies tok (== text)

nameStartsWith :: TokenVal -> Text -> Bool
nameStartsWith tok text = tokenNameSatisfies tok (text `T.isPrefixOf`)

nameEndsWith :: TokenVal -> Text -> Bool
nameEndsWith tok text = tokenNameSatisfies tok (text `T.isSuffixOf`)

tokenNameSatisfies :: TokenVal -> (Text -> Bool) -> Bool
tokenNameSatisfies (Token _ name) pred = pred name
tokenNameSatisfies _              _    = False

dropUntil :: Text -> [Token] -> [Token]
dropUntil token = tailSafe . dropWhile (not . (`hasName` token) . valOf)

-- * misc

dropDups :: (a -> a -> Bool) -> [a] -> [a]
dropDups cmp (x:xs) = go x xs
  where
    go a [] = [a]
    go a (b:bs)
      | cmp a b   = go a bs
      | otherwise = a: go b bs
dropDups _ [] = []

sortOn :: (Ord k) => (a -> k) -> [a] -> [a]
sortOn key = L.sortBy (compare `on` key)

printErr :: String -> IO ()
printErr = IO.hPutStrLn IO.stderr

-- split list into chunks delimited by specified element
split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split x xs = xs': split x (tailSafe xs'')
    where
      (xs', xs'') = break (==x) xs

tailSafe :: [a] -> [a]
tailSafe []     = []
tailSafe (_:xs) = xs
