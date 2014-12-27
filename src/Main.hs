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

module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import System.Console.GetOpt
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.Exit
import System.FilePath ((</>))

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Environment as Environment
import qualified System.IO as IO

import FastTags

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
        trackPrefixes = emacs
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
        (newTags, warnings) <- processFile ignoreEncErrs fn trackPrefixes
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

    printErr :: String -> IO ()
    printErr = IO.hPutStrLn IO.stderr

    contents recurse = if recurse
                         then getRecursiveDirContents
                         else getProperDirContents

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

