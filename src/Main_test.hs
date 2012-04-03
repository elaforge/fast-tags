{-# LANGUAGE OverloadedStrings #-}
module Main_test where
import Control.Monad
import qualified Data.Text as Text

import qualified Main as Main
import Main (Token, TokenVal(..), TagVal(..), Type(..))


-- This is kind of annoying without line numbers on equal and automatic
-- test_* collection...
main = do
    test_tokenize
    test_breakString
    test_stripComments
    test_process

test_tokenize = do
    -- drop leading "nl 0"
    let f = drop 1 . extractTokens . tokenize
    equal (f "a::b->c") ["a", "::", "b", "->", "c"]
    equal (f "x{-\n  bc#-}\n")
        ["x", "{-", "nl 2", "bc", "#", "-}"]
    equal (f "X.Y") ["X.Y"]
    equal (f "x9") ["x9"]
    -- equal (f "9x") ["nl 0", "9", "x"]
    equal (f "x :+: y") ["x", ":+:", "y"]
    equal (f "(#$)") ["(#$)"]
    equal (f "$#-- hi") ["$#", "--", "hi"]
    equal (f "(*), (-)") ["(*)", ",", "(-)"]

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
    let f = map extractTokens . map Main.stripNewlines
            . Main.breakBlocks . tokenize
    equal (f "1\n2\n") [["1"], ["2"]]
    equal (f "1\n 1\n2\n") [["1", "1"], ["2"]]
    equal (f "1\n 1\n 1\n2\n") [["1", "1", "1"], ["2"]]
    -- intervening blank lines are ignored
    equal (f "1\n 1\n\n 1\n2\n") [["1", "1", "1"], ["2"]]
    equal (f "1\n\n\n 1\n2\n") [["1", "1"], ["2"]]

    equal (f "1\n 11\n 11\n") [["1", "11", "11"]]
    equal (f " 11\n 11\n") [["11"], ["11"]]

test_process = test_misc >> test_data >> test_functions >> test_class

test_misc = do
    let f = process
    equal (f "module Bar.Foo where\n") [Tag "Foo" Module]
    equal (f "newtype Foo a b =\n\tBar x y z\n")
        [Tag "Foo" Type, Tag "Bar" Constructor]
    equal (f "f :: A -> B\ng :: C -> D\ndata D = C {\n\tf :: A\n\t}\n")
        [Tag "f" Function, Tag "g" Function, Tag "D" Type,
            Tag "C" Constructor, Tag "f" Function]

test_data = do
    let f = map untag . process
    equal (f "data X\n") ["X"]
    -- The extra X is suppressed.
    equal (f "data X = X Int\n") ["X"]
    equal (f "data Foo = Bar | Baz") ["Foo", "Bar", "Baz"]
    equal (f "data Foo =\n\tBar\n\t| Baz") ["Foo", "Bar", "Baz"]
    -- Records.
    equal (f "data Foo a = Bar { field :: Field }") ["Foo", "Bar", "field"]
    equal (f "data R = R { a::X, b::Y }") ["R", "a", "b"]
    equal (f "data R = R {\n\ta::X\n\t, b::Y\n\t}") ["R", "a", "b"]
    equal (f "data R = R {\n\ta,b::X\n\t}") ["R", "a", "b"]

test_functions = do
    let f = process
    -- Multiple declarations.
    equal (f "a,b::X") [Tag "a" Function, Tag "b" Function]
    -- With an operator.
    equal (f "(+), a :: X") [Tag "+" Function, Tag "a" Function]
    -- Don't get fooled by literals.
    equal (f "1 :: Int") []

test_class = do
    let f = map untag . process
    equal (process "class (X x) => C a b where\n\tm :: a->b\n\tn :: c\n")
        [Tag "C" Class, Tag "m" Function, Tag "n" Function]
    equal (f "class A a where f :: X\n") ["A", "f"]
    -- indented inside where
    equal (f "class X where\n\ta, (+) :: X\n") ["X", "a", "+"]
    equal (f "class X where\n\ta :: X\n\tb, c :: Y") ["X", "a", "b", "c"]
    equal (f "class X\n\twhere\n\ta :: X\n\tb, c :: Y") ["X", "a", "b", "c"]
    equal (f "class X\n\twhere\n\ta ::\n\t\tX\n\tb :: Y") ["X", "a", "b"]

process :: Text.Text -> [TagVal]
process = map Main.valOf . Main.process "fn"

untag :: TagVal -> String
untag (Tag name _) = Text.unpack name
untag (Warning warn) = "warn: " ++ warn

tokenize :: Text.Text -> [Token]
tokenize = concat . map Main.tokenize . Main.stripCpp . Main.annotate "fn"

plist :: (Show a) => [a] -> IO ()
plist xs = mapM_ (putStrLn . show) xs >> putChar '\n'

extractTokens :: [Token] -> [Text.Text]
extractTokens = map $ \token -> case Main.valOf token of
    Token name -> name
    Newline n -> Text.pack ("nl " ++ show n)

equal :: (Show a, Eq a) => a -> a -> IO ()
equal x y = unless (x == y) $
    putStrLn $ "__: " ++ show x ++ " /= " ++ show y
