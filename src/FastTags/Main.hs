{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

{- | Tagify haskell source.

    The key features are to be fast, incremental (i.e. merge tags with one
    file into existing tags), work on partially edited code, and work with
    hsc. That way I can hook it to the editor's save action and always keep
    the tags up to date.
-}
module FastTags.Main (main) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import Control.Monad

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Version as Version

import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO

import qualified FastTags.Emacs as Emacs
import qualified FastTags.Tag as Tag
import qualified FastTags.Token as Token
import qualified FastTags.Util as Util
import qualified FastTags.Vim as Vim

import qualified Paths_fast_tags


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
    , GetOpt.Option [] ["qualified"] (GetOpt.NoArg Qualified) $ concat
        [ "Each tag gets a version qualified with its module name, like M.f,"
        , " and an unqualified version."
        ]
    , GetOpt.Option [] ["fully-qualified"] (GetOpt.NoArg FullyQualified) $
        concat
        [ "Like --qualified, but the tag is fully qualified, A.B.C.f."
        , " Use with qualified_tag.py."
        ]
    ]

help :: String
help = unlines
    [ "usage: fast-tags [options] [filenames]"
    , ""
    , "If a single '-' is given for filenames, fast-tags expects a list of"
    , "files separated by newlines on stdin."
    , ""
    , "A tag will suppress any other tags with the same name within 2"
    , "lines.  This should prevent multiple tag matches for things like"
    , "`data X = X`.  Currently the 2 line limit is not configurable."
    ]

-- | Suppress tags with the same name within this number of lines.
maxSeparation :: Int
maxSeparation = 2

data Flag = Output FilePath | Help | Verbose | ETags | Recurse | NoMerge
    | ZeroSep | Version | NoModuleTags | Qualified | FullyQualified
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
                then Text.lines <$> Util.readFileLenient output
                else return []
        else return [] -- we do not support tags merging for emacs for now

    inputs <- Util.unique <$> getInputs flags inputs
    when (null inputs) $
        Exit.exitSuccess
    stderr <- MVar.newMVar IO.stderr
    newTags <- flip Async.mapConcurrently (zip [0..] inputs) $
        \(i :: Int, fn) -> do
            (newTags, warnings) <- Tag.processFile fn trackPrefixes
            newTags <- return $ if NoModuleTags `elem` flags
                then filter ((/=Tag.Module) . typeOf) newTags else newTags
            newTags <- return $ (newTags ++) $ if
                | FullyQualified `elem` flags -> map (Tag.qualify True) newTags
                | Qualified `elem` flags -> map (Tag.qualify False) newTags
                | otherwise -> []
            -- Try to do work before taking the lock.
            Exception.evaluate $ DeepSeq.rnf warnings
            MVar.withMVar stderr $ \hdl ->
                mapM_ (IO.hPutStrLn hdl) warnings
            when verbose $ do
                let line = take 78 $ show i ++ ": " ++ fn
                putStr $ '\r' : line ++ replicate (78 - length line) ' '
                IO.hFlush IO.stdout
            return newTags

    when verbose $ putChar '\n'

    let allTags = if vim
            then Vim.merge maxSeparation inputs newTags oldTags
            else Emacs.format maxSeparation (concat newTags)
    let write = if vim then Text.IO.hPutStrLn else Text.IO.hPutStr
    let withOutput action = if output == "-"
            then action IO.stdout
            else IO.withFile output IO.WriteMode action
    withOutput $ \hdl -> do
      IO.hSetEncoding hdl IO.utf8
      mapM_ (write hdl) allTags

    where
    usage msg = do
        putStr $ GetOpt.usageInfo (msg <> "\n" <> help) options
        Exit.exitFailure

typeOf :: Token.Pos Tag.TagVal -> Tag.Type
typeOf tagVal = case Token.valOf tagVal of
    Tag.TagVal _ typ -> typ

-- | Expand file inputs from cmdline.
getInputs :: [Flag] -> [FilePath] -> IO [FilePath]
getInputs flags inputs
    | inputs == ["-"] = Util.split sep <$> getContents
    | Recurse `elem` flags = fmap concat $ forM inputs $ \input -> do
        isDirectory <- Directory.doesDirectoryExist input
        if isDirectory
            then filter Tag.isHsFile <$> getRecursiveDirContents input
            else return [input]
    | otherwise = return inputs
    where
    sep = if ZeroSep `elem` flags then '\0' else '\n'

-- | Recurse directories collecting all files
getRecursiveDirContents :: FilePath -> IO [FilePath]
getRecursiveDirContents topdir = do
    paths <- listDir topdir
    fmap concat $ forM paths $ \path -> do
        isDirectory <- Directory.doesDirectoryExist path
        if isDirectory
            then getRecursiveDirContents path
            else return [path]

-- | 'Directory.getDirectoryContents', but don't return dot files, and prepend
-- directory.
listDir :: FilePath -> IO [FilePath]
listDir dir = map (dir</>) . filter (not . ("." `List.isPrefixOf`)) <$>
    Directory.getDirectoryContents dir
