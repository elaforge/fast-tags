{-# LANGUAGE ScopedTypeVariables #-}
{- | Tagify haskell source.

    The key features are to be fast, incremental (i.e. merge tags with one
    file into existing tags), work on partially edited code, and work with
    hsc. That way I can hook it to the editor's save action and always keep
    the tags up to date.
-}
module Main where
import Control.Monad
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit
import qualified System.IO as IO

import FastTags


main :: IO ()
main = do
    args <- Environment.getArgs
    (flags, inputs) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, inputs, []) -> return (flags, inputs)
        (_, _, errs) -> usage $ "flag errors:\n" ++ List.intercalate ", " errs
    let output = last $ "tags" : [fn | Output fn <- flags]
        verbose = Verbose `elem` flags
    oldTags <- fmap (maybe [vimMagicLine] Text.lines) $
        catchENOENT $ Text.IO.readFile output
    -- This will merge and sort the new tags.  But I don't run it on the
    -- the result of merging the old and new tags, so tags from another
    -- file won't be sorted properly.  To do that I'd have to parse all the
    -- old tags and run processAll on all of them, which is a hassle.
    -- TODO try it and see if it really hurts performance that much.
    newTags <- fmap (processAll . concat) $
        forM (zip [0..] inputs) $ \(i :: Int, fn) -> do
            tags <- processFile fn
            -- This has the side-effect of forcing the the tags, which is
            -- essential if I'm tagging a lot of files at once.
            let (warnings, newTags) = Either.partitionEithers tags
            forM_ warnings $ \warn -> do
                IO.hPutStrLn IO.stderr warn
            when verbose $ do
                let line = show i ++ " of " ++ show (length inputs - 1)
                        ++ ": " ++ fn
                putStr $ '\r' : line ++ replicate (78 - length line) ' '
                IO.hFlush IO.stdout
            return newTags
    when verbose $ putChar '\n'

    let write = if output == "-" then Text.IO.hPutStr IO.stdout
            else Text.IO.writeFile output
    write $ Text.unlines (mergeTags inputs oldTags newTags)
    where
    usage msg = putStr (GetOpt.usageInfo msg options)
        >> System.Exit.exitSuccess

data Flag = Output FilePath | Verbose
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option ['o'] [] (GetOpt.ReqArg Output "filename")
        "output file, defaults to 'tags'"
    , GetOpt.Option ['v'] [] (GetOpt.NoArg Verbose)
        "print files as they are tagged, useful to track down slow files"
    ]
