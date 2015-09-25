{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | Tagify haskell source.

    The key features are to be fast, incremental (i.e. merge tags with one
    file into existing tags), work on partially edited code, and work with
    hsc. That way I can hook it to the editor's save action and always keep
    the tags up to date.
-}
module Main (main) where
import Control.Applicative
import Control.Monad
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import System.Console.GetOpt
import System.Directory
    (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import qualified System.Exit as Exit
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.IO as IO
import qualified Data.Version as Version

import FastTags
import qualified Paths_fast_tags


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
        "expect list of file names on stdin to be 0-separated."
    , Option [] ["nomerge"] (NoArg NoMerge)
        "do not merge tag files"
    , Option [] ["version"] (NoArg Version) "print current version"
    ]

data Flag = Output FilePath | Help | Verbose | ETags | Recurse | NoMerge
    | ZeroSep | Version
    deriving (Eq, Show)

main :: IO ()
main = do
    args <- Environment.getArgs

    (flags, inputs) <- case getOpt Permute options args of
        (flags, inputs, []) -> return (flags, inputs)
        (_, _, errs) ->
            let errMsg = "flag errors:\n" ++ List.intercalate ", " errs
            in usage $ errMsg ++ "\n" ++ help

    when (Help `elem` flags) $ usage help
    when (Version `elem` flags) $ do
        putStrLn $ "fast-tags, version "
            ++ Version.showVersion Paths_fast_tags.version
        Exit.exitSuccess

    let verbose       = Verbose `elem` flags
        emacs         = ETags `elem` flags
        vim           = not emacs
        trackPrefixes = emacs
        recurse       = Recurse `elem` flags
        output        = last $ defaultOutput : [fn | Output fn <- flags]
        noMerge       = NoMerge `elem` flags
        useZeroSep    = ZeroSep `elem` flags
        sep           = if useZeroSep then '\0' else '\n'
        defaultOutput = if vim then "tags" else "TAGS"
        inputsM       = if null inputs
                        then split sep <$> getContents
                        else fmap concat $ forM inputs $ \input -> do
                            -- if an input is a directory then we find the
                            -- haskell files inside it, optionally recursing
                            -- further if the -R switch is specified
                            isDirectory <- doesDirectoryExist input
                            if isDirectory
                                then filter isHsFile <$> contents recurse input
                                else return [input]

    oldTags <- if vim && not noMerge
        then do
            exists <- doesFileExist output
            if exists
                then Text.lines <$> Text.IO.readFile output
                else return [vimMagicLine]
        else return [] -- we do not support tags merging for emacs for now

    inputs <- inputsM
    when (null inputs) $
        usage "no input files on either command line or stdin\n"
    -- This will merge and sort the new tags.  But I don't run it on the
    -- the result of merging the old and new tags, so tags from another
    -- file won't be sorted properly.  To do that I'd have to parse all the
    -- old tags and run processAll on all of them, which is a hassle.
    -- TODO try it and see if it really hurts performance that much.
    newTags <- fmap processAll $
        forM (zip [0..] inputs) $ \(i :: Int, fn) -> do
            (newTags, warnings) <- processFile fn trackPrefixes
            forM_ warnings printErr
            when verbose $ do
                let line = take 78 $ show i ++ ": " ++ fn
                putStr $ '\r' : line ++ replicate (78 - length line) ' '
                IO.hFlush IO.stdout
            return newTags

    when verbose $ putChar '\n'

    let write = if output == "-"
            then Text.IO.hPutStr IO.stdout
            else Text.IO.writeFile output

    write $ if vim
        then Text.unlines $ mergeTags inputs oldTags newTags
        else Text.concat $ prepareEmacsTags newTags

    where
    usage msg = putStr (GetOpt.usageInfo msg options) >> Exit.exitFailure

    printErr :: String -> IO ()
    printErr = IO.hPutStrLn IO.stderr

    contents recurse =
        if recurse then getRecursiveDirContents else getProperDirContents

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
    paths <- getProperDirContents topdir
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
printTagsTable = map (uncurry printSection) . Map.assocs

printSection :: FilePath -> [Pos TagVal] -> Text
printSection file tags = Text.concat
    ["\x0c\x0a", Text.pack file, ","
    , Text.pack $ show tagsLength, "\x0a", tagsText
    ]
    where
    tagsText = Text.unlines $ map printEmacsTag tags
    tagsLength = Text.length tagsText

printEmacsTag :: Pos TagVal -> Text
printEmacsTag (Pos (SrcPos _file line) (TagVal prefix _text _type)) =
  Text.concat [prefix, "\x7f", Text.pack (show line)]

classifyTagsByFile :: [Pos TagVal] -> TagsTable
classifyTagsByFile = foldr insertTag Map.empty

insertTag :: Pos TagVal -> TagsTable -> TagsTable
insertTag tag@(Pos (SrcPos file _) _) table =
    Map.insertWith (<>) file [tag] table

mergeTags :: [FilePath] -> [Text] -> [Pos TagVal] -> [Text]
mergeTags inputs old new =
    -- 'new' was already been sorted by 'process', but then I just concat
    -- the tags from each file, so they need sorting again.
    merge (map showTag new) (filter (not . isNewTag textFns) old)
    where
    textFns = Set.fromList $ map Text.pack inputs

help :: String
help = "usage: fast-tags [options] [filenames]\n" ++
       "In case no filenames provided on commandline, fast-tags expects " ++
       "list of files separated by newlines in stdin."

-- | Documented in vim :h tags-file-format.
-- This tells vim that the file is sorted (but not case folded) so that
-- it can do a bsearch and never needs to fall back to linear search.
vimMagicLine :: Text
vimMagicLine = "!_TAG_FILE_SORTED\t1\t~"

isNewTag :: Set Text -> Text -> Bool
isNewTag textFns line = Set.member fn textFns
    where
    fn = Text.takeWhile (/='\t') $ Text.drop 1 $ Text.dropWhile (/='\t') line

-- | Convert a Tag to text, e.g.: AbsoluteMark\tCmd/TimeStep.hs 67 ;" f
showTag :: Pos TagVal -> Text
showTag (Pos (SrcPos fn lineno) (TagVal _ text typ)) = Text.concat
    [ text, "\t"
    , Text.pack fn, "\t"
    , Text.pack (show lineno), ";\"\t"
    , Text.singleton (showType typ)
    ]

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
