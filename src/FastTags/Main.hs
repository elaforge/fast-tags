{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | Tagify haskell source.

    The key features are to be fast, incremental (i.e. merge tags with one
    file into existing tags), work on partially edited code, and work with
    hsc. That way I can hook it to the editor's save action and always keep
    the tags up to date.
-}
module FastTags.Main (main) where
import Control.Applicative
import qualified Control.Concurrent.Async as Async
import Control.Monad

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import qualified Data.Version as Version

import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO

import qualified FastTags.Tag as Tag
import qualified Paths_fast_tags
import qualified FastTags.Token as Token


options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option ['h'] ["help"] (GetOpt.NoArg Help)
        "print help message"
    , GetOpt.Option ['o'] [] (GetOpt.ReqArg Output "file")
        "output file, defaults to 'tags'"
    , GetOpt.Option ['e'] [] (GetOpt.NoArg ETags)
        "generate tags in Emacs format"
    , GetOpt.Option ['v'] [] (GetOpt.NoArg Verbose)
        "print files as they are tagged, useful to track down slow files"
    , GetOpt.Option ['R'] [] (GetOpt.NoArg Recurse)
        "read all files under any specified directories recursively"
    , GetOpt.Option ['0'] [] (GetOpt.NoArg ZeroSep)
        "expect list of file names on stdin to be 0-separated."
    , GetOpt.Option [] ["nomerge"] (GetOpt.NoArg NoMerge)
        "replace an existing tags file instead of merging into it"
    , GetOpt.Option [] ["version"] (GetOpt.NoArg Version)
        "print current version"
    , GetOpt.Option [] ["no-module-tags"] (GetOpt.NoArg NoModuleTags)
        "do not generate tags for modules"
    ]

data Flag = Output FilePath | Help | Verbose | ETags | Recurse | NoMerge
    | ZeroSep | Version | NoModuleTags
    deriving (Eq, Show)

main :: IO ()
main = do
    args <- Environment.getArgs
    (flags, inputs) <- case GetOpt.getOpt GetOpt.Permute options args of
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
        output        = last $ defaultOutput : [fn | Output fn <- flags]
        defaultOutput = if vim then "tags" else "TAGS"

    oldTags <- if vim && NoMerge `notElem` flags
        then do
            exists <- Directory.doesFileExist output
            if exists
                then Text.lines <$> Text.IO.readFile output
                else return [vimMagicLine]
        else return [] -- we do not support tags merging for emacs for now

    inputs <- getInputs flags inputs
    when (null inputs) $
        usage "no input files on either command line or stdin\n"
    -- This will merge and sort the new tags.  But I don't run it on the
    -- the result of merging the old and new tags, so tags from another
    -- file won't be sorted properly.  To do that I'd have to parse all the
    -- old tags and run postProcess on all of them, which is a hassle.
    -- TODO try it and see if it really hurts performance that much.
    -- TODO I think this is only for the type order?  Because I do keep
    -- everything sorted.
    newTags <- fmap Tag.postProcess $
        flip Async.mapConcurrently (zip [0..] inputs) $ \(i :: Int, fn) -> do
            (newTags, warnings) <- Tag.processFile fn trackPrefixes
            newTags <- return $ if NoModuleTags `elem` flags
                then filter ((/=Tag.Module) . typeOf) newTags
                else newTags
            mapM_ (IO.hPutStrLn IO.stderr) warnings
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

typeOf :: Token.Pos Tag.TagVal -> Tag.Type
typeOf tagVal = case Token.valOf tagVal of
    Tag.TagVal _ typ -> typ

-- | Expand file inputs. If there are no inputs, read them from stdin.  For
-- directories, get *.hs inside, and continue to recurse if Recurse is set.
getInputs :: [Flag] -> [FilePath] -> IO [FilePath]
getInputs flags inputs
    | null inputs = Tag.split sep <$> getContents
    | otherwise = fmap concat $ forM inputs $ \input -> do
        -- If an input is a directory then we find the haskell files inside it,
        -- optionally recursing further if the -R switch is specified.
        isDirectory <- Directory.doesDirectoryExist input
        if isDirectory
            then filter Tag.isHsFile <$> contents input
            else return [input]
    where
    contents
        | Recurse `elem` flags = getRecursiveDirContents
        | otherwise = getProperDirContents
    sep = if ZeroSep `elem` flags then '\0' else '\n'

-- | Get all absolute filepaths contained in the supplied topdir,
-- except "." and ".."
getProperDirContents :: FilePath -> IO [FilePath]
getProperDirContents topdir = do
    names <- Directory.getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    return $ map ((</>) topdir) properNames

-- | Recurse directories collecting all files
getRecursiveDirContents :: FilePath -> IO [FilePath]
getRecursiveDirContents topdir = do
    paths <- getProperDirContents topdir
    paths' <- forM paths $ \path -> do
        isDirectory <- Directory.doesDirectoryExist path
        if isDirectory
            then getRecursiveDirContents path
            else return [path]
    return (concat paths')


type TagsTable = Map FilePath [Token.Pos Tag.TagVal]

prepareEmacsTags :: [Token.Pos Tag.TagVal] -> [Text]
prepareEmacsTags = printTagsTable . classifyTagsByFile

printTagsTable :: TagsTable -> [Text]
printTagsTable = map (uncurry printSection) . Map.assocs

printSection :: FilePath -> [Token.Pos Tag.TagVal] -> Text
printSection file tags = Text.concat
    ["\x0c\x0a", Text.pack file, ","
    , Text.pack $ show tagsLength, "\x0a", tagsText
    ]
    where
    tagsText = Text.unlines $ map printEmacsTag tags
    tagsLength = Text.length tagsText

printEmacsTag :: Token.Pos Tag.TagVal -> Text
printEmacsTag (Token.Pos pos (Tag.TagVal {})) = Text.concat
    [ Token.posPrefix pos
    , "\x7f"
    , Text.pack (show $ Token.unLine (Token.posLine pos))
    ]

classifyTagsByFile :: [Token.Pos Tag.TagVal] -> TagsTable
classifyTagsByFile = foldr insertTag Map.empty

insertTag :: Token.Pos Tag.TagVal -> TagsTable -> TagsTable
insertTag tag@(Token.Pos pos _) table =
    Map.insertWith (<>) (Token.posFile pos) [tag] table

mergeTags :: [FilePath] -> [Text] -> [Token.Pos Tag.TagVal] -> [Text]
mergeTags inputs old new =
    -- 'new' was already been sorted by 'process', but then I just concat
    -- the tags from each file, so they need sorting again.
    Tag.merge (map showTag new) (filter (not . isNewTag textFns) old)
    where
    textFns = Set.fromList $ map Text.pack inputs

help :: String
help = "usage: fast-tags [options] [filenames]\n" ++
       "In case no filenames provided on commandline, fast-tags expects " ++
       "list of files separated by newlines in stdin."

-- | This line is to tell vim that the file is sorted, so it can use binary
-- search when looking for tags. This must come first in the tags file, and the
-- format is documented in :h tags-file-format as:
--
--   !_TAG_FILE_SORTED<Tab>1<Tab>{anything}
--
-- However, simply leaving {anything} part empty or putting something random
-- like ~ doesn't work when we want to extend the tags file with some tags from
-- C files using ctags. ctags requires //, with optional comments in between two
-- slashes. More about ctags' file format can be seen here:
-- http://ctags.sourceforge.net/FORMAT.
vimMagicLine :: Text
vimMagicLine = "!_TAG_FILE_SORTED\t1\t//"

isNewTag :: Set Text -> Text -> Bool
isNewTag textFns line = Set.member fn textFns
    where
    fn = Text.takeWhile (/='\t') $ Text.drop 1 $ Text.dropWhile (/='\t') line

-- | Convert a Tag to text, e.g.: AbsoluteMark\tCmd/TimeStep.hs 67 ;" f
showTag :: Token.Pos Tag.TagVal -> Text
showTag (Token.Pos pos (Tag.TagVal text typ)) = mconcat
    [ text, "\t"
    , Text.pack (Token.posFile pos), "\t"
    , Text.pack (show $ Token.unLine (Token.posLine pos)), ";\"\t"
    , Text.singleton (showType typ)
    ]

-- | Vim takes this to be the \"kind:\" annotation.  It's just an arbitrary
-- string and these letters conform to no standard.  Presumably there are some
-- vim extensions that can make use of it.
showType :: Tag.Type -> Char
showType typ = case typ of
    Tag.Module      -> 'm'
    Tag.Function    -> 'f'
    Tag.Class       -> 'c'
    Tag.Type        -> 't'
    Tag.Constructor -> 'C'
    Tag.Operator    -> 'o'
    Tag.Pattern     -> 'p'
