{-# LANGUAGE OverloadedStrings #-}
module Main_test where
import Control.Monad
import qualified System.IO.Unsafe as Unsafe
import qualified Data.Text as Text
import qualified Control.Exception as Exception
import Exception (assert)

import qualified Main as Main
import Main (Token, TokenVal(..), TagVal(..), Type(..))


-- This is kind of annoying without automatic test_* collection...
main = do
    test_tokenize
    test_breakString
    test_stripComments
    test_process

test_tokenize = do
    -- drop leading "nl 0"
    let f = drop 1 . extractTokens . tokenize
    equal assert (f "a::b->c") ["a", "::", "b", "->", "c"]
    equal assert (f "x{-\n  bc#-}\n")
        ["x", "{-", "nl 2", "bc", "#", "-}"]
    equal assert (f "X.Y") ["X.Y"]
    equal assert (f "x9") ["x9"]
    -- equal assert (f "9x") ["nl 0", "9", "x"]
    equal assert (f "x :+: y") ["x", ":+:", "y"]
    equal assert (f "(#$)") ["(#$)"]
    equal assert (f "$#-- hi") ["$#", "--", "hi"]
    equal assert (f "(*), (-)") ["(*)", ",", "(-)"]

test_breakString = do
    let f = Main.breakString
    equal assert (f "hi \" there") ("hi \"", " there")
    equal assert (f "hi \\a \" there") ("hi \\a \"", " there")
    equal assert (f "hi \\\" there\"") ("hi \\\" there\"", "")
    equal assert (f "hi") ("hi", "")
    -- String continuation isn't implemented yet.
    equal assert (f "hi \\") ("hi \\", "")

test_stripComments = do
    let f = extractTokens . Main.stripComments . tokenize
    equal assert (f "hello -- there") ["nl 0", "hello"]
    equal assert (f "hello {- there -} fred") ["nl 0", "hello", "fred"]
    equal assert (f "{-# LANG #-} hello {- there {- nested -} comment -} fred")
        ["nl 0", "hello", "fred"]

test_breakBlocks = do
    let f = map extractTokens . map Main.stripNewlines
            . Main.breakBlocks . tokenize
    equal assert (f "1\n2\n") [["1"], ["2"]]
    equal assert (f "1\n 1\n2\n") [["1", "1"], ["2"]]
    equal assert (f "1\n 1\n 1\n2\n") [["1", "1", "1"], ["2"]]
    -- intervening blank lines are ignored
    equal assert (f "1\n 1\n\n 1\n2\n") [["1", "1", "1"], ["2"]]
    equal assert (f "1\n\n\n 1\n2\n") [["1", "1"], ["2"]]

    equal assert (f "1\n 11\n 11\n") [["1", "11", "11"]]
    equal assert (f " 11\n 11\n") [["11"], ["11"]]

test_process = test_misc >> test_data >> test_functions >> test_class

test_misc = do
    let f = process
    equal assert (f "module Bar.Foo where\n") [Tag "Foo" Module]
    equal assert (f "newtype Foo a b =\n\tBar x y z\n")
        [Tag "Foo" Type, Tag "Bar" Constructor]
    equal assert (f "f :: A -> B\ng :: C -> D\ndata D = C {\n\tf :: A\n\t}\n")
        [Tag "f" Function, Tag "g" Function, Tag "D" Type,
            Tag "C" Constructor, Tag "f" Function]

test_data = do
    let f = map untag . process
    equal assert (f "data X\n") ["X"]
    -- The extra X is suppressed.
    equal assert (f "data X = X Int\n") ["X"]
    equal assert (f "data Foo = Bar | Baz") ["Foo", "Bar", "Baz"]
    equal assert (f "data Foo =\n\tBar\n\t| Baz") ["Foo", "Bar", "Baz"]
    -- Records.
    equal assert (f "data Foo a = Bar { field :: Field }")
        ["Foo", "Bar", "field"]
    equal assert (f "data R = R { a::X, b::Y }") ["R", "a", "b"]
    equal assert (f "data R = R {\n\ta::X\n\t, b::Y\n\t}") ["R", "a", "b"]
    equal assert (f "data R = R {\n\ta,b::X\n\t}") ["R", "a", "b"]

    equal assert (f "data R = R {\n\ta :: !RealTime\n\t, b :: !RealTime\n\t}")
        ["R", "a", "b"]


test_functions = do
    let f = process
    -- Multiple declarations.
    equal assert (f "a,b::X") [Tag "a" Function, Tag "b" Function]
    -- With an operator.
    equal assert (f "(+), a :: X") [Tag "+" Function, Tag "a" Function]
    -- Don't get fooled by literals.
    equal assert (f "1 :: Int") []

test_class = do
    let f = map untag . process
    equal assert (process "class (X x) => C a b where\n\tm :: a->b\n\tn :: c\n")
        [Tag "C" Class, Tag "m" Function, Tag "n" Function]
    equal assert (f "class A a where f :: X\n") ["A", "f"]
    -- indented inside where
    equal assert (f "class X where\n\ta, (+) :: X\n") ["X", "a", "+"]
    equal assert (f "class X where\n\ta :: X\n\tb, c :: Y")
        ["X", "a", "b", "c"]
    equal assert (f "class X\n\twhere\n\ta :: X\n\tb, c :: Y")
        ["X", "a", "b", "c"]
    equal assert (f "class X\n\twhere\n\ta ::\n\t\tX\n\tb :: Y")
        ["X", "a", "b"]

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

equal :: (Show a, Eq a) => Assert z -> a -> a -> IO ()
equal srcpos x y = unless (x == y) $
    putStrLn $ "__ " ++ getSourceLoc srcpos ++ " " ++ show x ++ " /= " ++ show y

type Assert a = Bool -> a -> String

-- | Awful ghc hack to get source line location.
getSourceLoc :: Assert a -> String
getSourceLoc assert_ = takeWhile (/=' ') $ Unsafe.unsafePerformIO $
    Exception.evaluate (assert_ False (error "Impossible"))
        `Exception.catch` (\(Exception.AssertionFailed s) -> return s)

