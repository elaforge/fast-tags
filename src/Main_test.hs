{-# LANGUAGE OverloadedStrings #-}
module Main_test where
import Control.Monad
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Main as Main
import Main (Token, TokenVal(..), TagVal(..), Type(..))


test_tokenize = do
    let f = extractTokens . tokenize
    equal (f "a::b->c") ["nl 0", "a", "::", "b", "->", "c"]
    equal (f "x{-\n  bc#-}\n")
        ["nl 0", "x", "{-", "nl 2", "bc", "#", "-}"]
    equal (f "X.Y") ["nl 0", "X.Y"]

test_breakString = do
    let f = Main.breakString
    equal (f "hi \" there") ("hi \"", " there")
    equal (f "hi \\a \" there") ("hi \\a \"", " there")
    equal (f "hi \\\" there\"") ("hi \\\" there\"", "")
    equal (f "hi") ("hi", "")
    -- String continuation isn't implemented yet.
    equal (f "hi \\") ("hi \\", "")

test_stripComments = do
    let f = extractTokens . Main.stripComments . tokenize
    equal (f "hello -- there") ["nl 0", "hello"]
    equal (f "hello {- there -} fred") ["nl 0", "hello", "fred"]
    equal (f "{-# LANG #-} hello {- there {- nested -} comment -} fred")
        ["nl 0", "hello", "fred"]

test_breakBlocks = do
    let f = map extractTokens . Main.breakBlocks . tokenize
    equal (f "1\n2\n") [["1"], ["2"]]
    equal (f "1\n 1\n2\n") [["1", "1"], ["2"]]
    equal (f "1\n 1\n 1\n2\n") [["1", "1", "1"], ["2"]]
    -- intervening blank lines are ignored
    equal (f "1\n 1\n\n 1\n2\n") [["1", "1", "1"], ["2"]]
    equal (f "1\n\n\n 1\n2\n") [["1", "1"], ["2"]]

test_process = do
    let f = map Main.valOf . Main.process "fn"
    equal (f "module Foo where\n") [Tag "Foo" Module]
    equal (f "newtype Foo a b =\n\tBar x y z\n")
        [Tag "Foo" Type, Tag "Bar" Constructor]
    equal (f "data Foo a = Bar { field :: Field }")
        [Tag "Foo" Type, Tag "Bar" Constructor, Tag "field" Function]
    equal (f "data Foo = Bar | Baz")
        [Tag "Foo" Type, Tag "Bar" Constructor, Tag "Baz" Constructor]
    equal (f "class (X x) => C a b c where\n\tm :: a -> b\n\tn :: c -> d\n")
        [Tag "C" Class, Tag "m" Function, Tag "n" Function]
    equal (f "class A a where f :: X\n")
        [Tag "A" Class, Tag "f" Function]
    equal (f "data X\n")
        [Tag "X" Type]

    equal (f "f :: A -> B\ng :: C -> D\ndata D = C {\n\tf :: A\n\t}\n")
        [Tag "f" Function, Tag "g" Function, Tag "D" Type,
            Tag "C" Constructor, Tag "f" Function]


tokenize :: Text.Text -> [Token]
tokenize = concat . map Main.tokenize . Main.stripCpp . Main.annotate "fn"

plist :: (Show a) => [a] -> IO ()
plist xs = mapM_ (putStrLn . show) xs >> putChar '\n'

extractTokens :: [Token] -> [Text.Text]
extractTokens = Maybe.mapMaybe $ \token -> case Main.valOf token of
    Token name -> Just name
    Newline n -> Just (Text.pack ("nl " ++ show n))

equal :: (Show a, Eq a) => a -> a -> IO ()
equal x y = unless (x == y) $
    putStrLn $ "__: " ++ show x ++ " /= " ++ show y
