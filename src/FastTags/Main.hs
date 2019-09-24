{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- | Tagify haskell source.

    The key features are to be fast, incremental (i.e. merge tags with one
    file into existing tags), work on partially edited code, and work with
    hsc. That way I can hook it to the editor's save action and always keep
    the tags up to date.
-}
module FastTags.Main (main) where
#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative
#endif
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import Control.Monad

import Control.Arrow (first, second)
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Version as Version

import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified System.IO as IO

import qualified FastTags.Cabal as Cabal
import qualified FastTags.Emacs as Emacs
import qualified FastTags.Tag as Tag
import qualified FastTags.Token as Token
import qualified FastTags.Util as Util
import qualified FastTags.Vim as Vim

import qualified Paths_fast_tags


options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["cabal"] (GetOpt.NoArg Cabal) $ concat
        [ "Parse the arguments as cabal files, then generate tags for exposed"
        , " modules.  Has the effect of adding --src-prefix as appropriate."
        , " This will fail on *.x or *.lhs files, generated"
        , " Paths_*.hs, or exposed-modules controlled by flags."
        , " Also, if there are multiple hs-source-dirs it will pick the first."
        , " (TODO: fix that)"
        , " This only works for >=Cabal-2.2.0."
        ]
    , GetOpt.Option ['e'] ["emacs"] (GetOpt.NoArg ETags)
        "generate tags in Emacs format"
    , GetOpt.Option [] ["exclude"] (GetOpt.ReqArg Exclude "pattern") $ concat
        [ "Add a pattern to a list of files to exclude when -R is given."
        , "  The pattern is matched against the basename and complete path of"
        , " each file and directory.  This features is based on exuberant"
        , " ctags."
        ]
    , GetOpt.Option ['h'] ["help"] (GetOpt.NoArg Help)
        "print help message"
    , GetOpt.Option ['L'] ["follow-symlinks"] (GetOpt.NoArg FollowSymlinks)
        "follow symlinks when -R is given"
    , GetOpt.Option [] ["nomerge"] (GetOpt.NoArg NoMerge)
        "replace an existing tags file instead of merging into it"
    , GetOpt.Option [] ["no-module-tags"] (GetOpt.NoArg NoModuleTags)
        "do not generate tags for modules"
    , GetOpt.Option ['o'] [] (GetOpt.ReqArg Output "file")
        "output file, defaults to 'tags'"
    , GetOpt.Option [] ["qualified"] (GetOpt.NoArg Qualified) $ concat
        [ "Each tag gets a version qualified with its module name, like M.f,"
        , " and an unqualified version."
        ]
    , GetOpt.Option [] ["fully-qualified"] (GetOpt.NoArg FullyQualified) $
        concat
        [ "Like --qualified, but the tag is fully qualified, A.B.C.f."
        , " Use with qualified_tag.py."
        ]
    , GetOpt.Option [] ["src-prefix"] (GetOpt.ReqArg SrcPrefix "path") $ concat
        [ "Strip this from the front of module names. This is useful if the"
        , " source is somewhere below the directory you run the editor in."
        , " This only has an effect for --fully-qualified."
        ]
    , GetOpt.Option ['R'] [] (GetOpt.NoArg Recurse)
        "read all files under any specified directories recursively"
    , GetOpt.Option ['v'] ["verbose"] (GetOpt.NoArg Verbose)
        "print files as they are tagged, useful to track down slow files"
    , GetOpt.Option [] ["version"] (GetOpt.NoArg Version)
        "print current version"
    , GetOpt.Option ['0'] [] (GetOpt.NoArg ZeroSep)
        "expect list of file names on stdin to be 0-separated."
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

type Pattern = String

data Flag =
    Cabal
    | ETags
    | Exclude !Pattern
    | FollowSymlinks
    | FullyQualified
    | Help
    | NoMerge
    | NoModuleTags
    | Output !FilePath
    | Qualified
    | Recurse
    | SrcPrefix !FilePath
    | Verbose
    | Version
    | ZeroSep
    deriving (Eq, Show)

main :: IO ()
main = do
    args <- Environment.getArgs
    (flags, inputs) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, inputs, []) -> return (flags, inputs)
        (_, _, errs) -> usage $ "flag errors:\n" ++ List.intercalate ", " errs

    when (Help `elem` flags) $ usage ""
    when (Version `elem` flags) $ do
        putStrLn $ "fast-tags, version "
            ++ Version.showVersion Paths_fast_tags.version
        Exit.exitSuccess

    let verbose       = Verbose `elem` flags
        emacs         = ETags `elem` flags
        vim           = not emacs
        trackPrefixes = emacs
        output        = last $ defaultOutput : [fn | Output fn <- flags]
        -- Put the longest prefixes first, so they don't shadow each other.
        srcPrefixes   = List.reverse $ List.sortOn Text.length
            [Text.pack (FilePath.normalise fn) | SrcPrefix fn <- flags]
        defaultOutput = if vim then "tags" else "TAGS"

    oldTags <- if vim && NoMerge `notElem` flags
        then do
            exists <- Directory.doesFileExist output
            if exists
                then Text.lines <$> Util.readFileLenient output
                else return []
        else return [] -- we do not support tags merging for emacs for now

    inputs <- if Cabal `elem` flags
        then map (first (:[])) <$> getCabalInputs inputs
        else map ((srcPrefixes,) . FilePath.normalise) . Util.unique <$>
            getInputs flags inputs
    when (null inputs) $
        Exit.exitSuccess

    -- Hack: cabal just lists the module name, which I turn into a filename, so
    -- I don't know if it actually is .hsc.  Or .lhs, but I can't parse those
    -- anyway.
    let tryHsc = Cabal `elem` flags
    stderr <- MVar.newMVar IO.stderr
    newTags <- flip Async.mapConcurrently (zip [0 :: Int ..] inputs) $
        \(i, (srcPrefixes, fn)) -> Exception.handle (catchError stderr fn) $ do
            useHsc <- if tryHsc then Directory.doesFileExist (fn ++ "c")
                else return False
            fn <- return $ if useHsc then fn ++ "c" else fn
            (newTags, warnings) <- Tag.processFile fn trackPrefixes
            newTags <- return $ if NoModuleTags `elem` flags
                then filter ((/=Tag.Module) . typeOf) newTags else newTags
            -- All of the tags from one file should have the same src-prefix,
            -- so save some work by finding it only once for the whole file.
            let qualify fully = map
                    (Tag.qualify fully
                        (maybe Nothing (Tag.findSrcPrefix srcPrefixes)
                            (Util.mhead newTags)))
                    newTags
            newTags <- return $ (newTags ++) $ if
                | FullyQualified `elem` flags -> qualify True
                | Qualified `elem` flags -> qualify False
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
            then Vim.merge maxSeparation (map snd inputs) newTags oldTags
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
        putStr $ GetOpt.usageInfo (msg ++ "\n" ++ help) options
        Exit.exitFailure

catchError :: MVar.MVar IO.Handle -> FilePath -> Exception.SomeException
    -> IO [a]
catchError stderr fn e = do
    MVar.withMVar stderr $ \hdl -> IO.hPutStrLn hdl $
        "Error while analyzing " ++ fn ++ ":\n" ++ show e
    return []

typeOf :: Token.Pos Tag.TagVal -> Tag.Type
typeOf tagVal = case Token.valOf tagVal of
    Tag.TagVal _ typ _ -> typ

-- | Expand file inputs from cmdline.
getInputs :: [Flag] -> [FilePath] -> IO [FilePath]
getInputs flags inputs
    | inputs == ["-"] = Util.split sep <$> getContents
    | Recurse `elem` flags = fmap concat $ forM inputs $ \input -> do
        isDirectory <- Directory.doesDirectoryExist input
        if isDirectory
            then filter Tag.isHsFile <$>
                getRecursiveDirContents followSymlinks
                    [pattern | Exclude pattern <- flags] input
            else return [input]
    | otherwise = return inputs
    where
    sep = if ZeroSep `elem` flags then '\0' else '\n'
    followSymlinks = FollowSymlinks `elem` flags

-- | Parse .cabal files.
getCabalInputs :: [FilePath] -> IO [(Text.Text, FilePath)]
    -- ^ [(hsSrcDir, modulePath)]
getCabalInputs fnames = do
    results <- mapM Cabal.parse fnames
    let (errs, srcMods) = partitionEitherSnd $ zip fnames results
    forM_ errs $ \(fname, err) ->
        IO.hPutStrLn IO.stderr $ "parsing " <> show fname <> ": " <> err
    return $ do
        (cabalFname, (hsSrcDir, mods)) <- srcMods
        let srcDir = FilePath.normalise $
                FilePath.takeDirectory cabalFname </> hsSrcDir
        mod <- mods
        return (Text.pack srcDir, srcDir </> mod)

-- | Recurse directories collecting all files
getRecursiveDirContents :: Bool -> [Pattern] -> FilePath -> IO [FilePath]
getRecursiveDirContents followSymlinks excludes = go
    where
    go topdir = do
        paths <- listDir topdir
        fmap concat $ forM (filter wanted paths) $ \path -> do
            isDirectory <- Directory.doesDirectoryExist path
            isSymlink <- Directory.pathIsSymbolicLink path
            if isDirectory && (followSymlinks || not isSymlink)
                then go path
                else return [path]
    wanted fname = not $ any (`patternMatches` fname) excludes

patternMatches :: Pattern -> FilePath -> Bool
patternMatches pattern fname =
    pattern == fname || pattern == FilePath.takeBaseName fname

partitionEitherSnd :: [(a, Either b c)] -> ([(a, b)], [(a, c)])
partitionEitherSnd = go
    where
    go ((a, Left b) : xs) = first ((a, b) :) (go xs)
    go ((a, Right c) : xs) = second ((a, c) :) (go xs)
    go [] = ([], [])

-- | Like 'Directory.listDirectory', but prepend the directory.
listDir :: FilePath -> IO [FilePath]
listDir dir =
    map (clean . (dir</>)) . filter (not . ("." `List.isPrefixOf`)) <$>
        Directory.listDirectory dir
    where
    clean ('.' : '/' : path) = path
    clean path = path
