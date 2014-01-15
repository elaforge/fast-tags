{-# LANGUAGE OverloadedStrings #-}
module Main_test where
import qualified Control.Exception as Exception
import Control.Monad
import qualified Data.Either as Either
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text

import Control.Exception (assert)
import qualified System.IO.Unsafe as Unsafe

import qualified Main as Main
import Main (TokenVal(..), TagVal(..), Type(..), Tag, Pos(..))


-- This is kind of annoying without automatic test_* collection...
main = sequence_
    [ test_tokenize, test_skipString, test_stripComments
    , test_breakBlocks, test_processAll
    , test_process
    ]

test_tokenize = do
    -- drop leading "nl 0"
    let f = drop 1 . extractTokens . tokenize
    equal assert (f "a::b->c") ["a", "::", "b", "->", "c"]
    equal assert (f "x{-\n  bc#-}\n")
        ["x", "{-", "nl 2", "bc#", "-}"]
    equal assert (f "X.Y") ["X.Y"]
    equal assert (f "x9") ["x9"]
    -- equal assert (f "9x") ["nl 0", "9", "x"]
    equal assert (f "x :+: y") ["x", ":+:", "y"]
    equal assert (f "(#$)") ["(", "#$", ")"]
    equal assert (f "$#-- hi") ["$#", "--", "hi"]
    equal assert (f "(*), (-)") ["(", "*", ")", ",", "(", "-", ")"]
    equal assert (f "(.::)") ["(", ".::", ")"]
    -- we rely on this behavior
    equal assert (f "data (:+) a b") ["data", "(", ":+", ")", "a", "b"]
    equal assert (f "data (.::+::) a b") ["data", "(", ".::+::", ")", "a", "b"]

test_skipString = do
    let f = snd . Main.breakString
    equal assert (f "hi \" there") " there"
    equal assert (f "hi \\a \" there") " there"
    equal assert (f "hi \\\" there\"") ""
    equal assert (f "hi") ""
    -- String continuation isn't implemented yet.
    equal assert (f "hi \\") ""

test_stripComments = do
    let f = extractTokens . Main.stripComments . tokenize
    equal assert (f "hello -- there") ["nl 0", "hello"]
    equal assert (f "hello {- there -} fred") ["nl 0", "hello", "fred"]
    equal assert (f "{-# LANG #-} hello {- there {- nested -} comment -} fred")
        ["nl 0", "hello", "fred"]

test_breakBlocks = do
    let f = map (extractTokens . Main.UnstrippedTokens . Main.stripNewlines)
            . Main.breakBlocks . tokenize
    equal assert (f "1\n2\n") [["1"], ["2"]]
    equal assert (f "1\n 1\n2\n") [["1", "1"], ["2"]]
    equal assert (f "1\n 1\n 1\n2\n") [["1", "1", "1"], ["2"]]
    -- intervening blank lines are ignored
    equal assert (f "1\n 1\n\n 1\n2\n") [["1", "1", "1"], ["2"]]
    equal assert (f "1\n\n\n 1\n2\n") [["1", "1"], ["2"]]

    equal assert (f "1\n 11\n 11\n") [["1", "11", "11"]]
    equal assert (f " 11\n 11\n") [["11"], ["11"]]

test_processAll = do
    let f = map showTag . Main.processAll . Either.rights
            . concatMap (\(i, t) -> Main.process ("fn" ++ show i) t)
            . zip [0..]
        showTag (Pos p (Tag _ text typ)) =
            unwords [show p, Text.unpack text, show typ]
    equal assert (f ["data X", "module X"])
        ["fn0:1 X Type", "fn1:1 X Module"]
    -- Type goes ahead of Module.
    equal assert (f ["module X\ndata X"])
        ["fn0:2 X Type", "fn0:1 X Module"]
    -- Extra X was filtered.
    equal assert (f ["module X\ndata X = X\n"])
        ["fn0:2 X Type", "fn0:1 X Module"]

test_process = sequence_
    [ test_misc, test_data, test_gadt, test_families, test_functions
    , test_class
    , test_literate
    ]

test_misc = do
    let f text = [tag | Right (Pos _ tag) <- Main.process "fn" text]
    equal assert (f "module Bar.Foo where\n")
        [Tag "module Bar.Foo" "Foo" Module]
    equal assert (f "newtype Foo a b =\n\tBar x y z\n")
        [Tag "newtype Foo" "Foo" Type, Tag "\tBar" "Bar" Constructor]
    equal assert (f "f :: A -> B\ng :: C -> D\ndata D = C {\n\tf :: A\n\t}\n")
        [Tag "f" "f" Function, Tag "g" "g" Function, Tag "data D" "D" Type,
            Tag "data D = C" "C" Constructor, Tag "\tf" "f" Function]

test_unicode = do
    let f = process
    equal assert (f "數字 :: Int") ["數字"]
    equal assert (f "(·), x :: Int") ["·", "x"]

test_data = do
    let f = process
    equal assert (f "data X\n") ["X"]
    equal assert (f "data X = X Int\n") ["X", "X"]
    equal assert (f "data Foo = Bar | Baz") ["Foo", "Bar", "Baz"]
    equal assert (f "data Foo =\n\tBar\n\t| Baz") ["Foo", "Bar", "Baz"]
    -- Records.
    equal assert (f "data Foo a = Bar { field :: Field }")
        ["Foo", "Bar", "field"]
    equal assert (f "data R = R { a::X, b::Y }") ["R", "R", "a", "b"]
    equal assert (f "data R = R {\n\ta::X\n\t, b::Y\n\t}") ["R", "R", "a", "b"]
    equal assert (f "data R = R {\n\ta,b::X\n\t}") ["R", "R", "a", "b"]

    equal assert (f "data R = R {\n\ta :: !RealTime\n\t, b :: !RealTime\n\t}")
        ["R", "R", "a", "b"]

    equal assert (f "data X = X !Int") ["X", "X"]
    equal assert (f "data X = Y !Int !X | Z") ["X", "Y", "Z"]
    equal assert (f "data X = Y :+: !Z | !X `Mult` X") ["X", ":+:", "Mult"]
    equal assert (f "data X = !Y `Add` !Z") ["X", "Add"]

    equal assert (f "newtype (u :*: v) z = X") [":*:", "X"]
    equal assert (f "data (u :*: v) z = X") [":*:", "X"]
    equal assert (f "type (u :*: v) z = (u, v, z)") [":*:"]


    equal assert (f "data (:*:) u v z = X") [":*:", "X"]
    equal assert (f "data (u `W` v) z = X") ["W", "X"]

    equal assert
        (f "newtype X a = Z {\n -- TODO blah\n foo :: [a] }")
        ["X", "Z", "foo"]
    equal assert
        (f "newtype (u :*: v) z = X {\n -- my insightful comment\n extract :: (u (v z)) }")
        [":*:", "X", "extract"]


test_gadt = do
    let f = process
    equal assert (f "data X where A :: X\n") ["X", "A"]
    equal assert (f "data X where\n\tA :: X\n") ["X", "A"]
    equal assert (f "data X where\n\tA :: X\n\tB :: X\n") ["X", "A", "B"]
    equal assert (f "data X where\n\tA, B :: X\n") ["X", "A", "B"]

test_families = do
    let f = process
    equal assert (f "type family X :: *\n") ["X"]
    equal assert (f "data family X :: * -> *\n") ["X"]
    equal assert (f "class C where\n\ttype X y :: *\n") ["C", "X"]
    equal assert (f "class C where\n\tdata X y :: *\n") ["C", "X"]

test_functions = do
    let f = process
    -- Multiple declarations.
    equal assert (f "a,b::X") ["a", "b"]
    -- With an operator.
    equal assert (f "(+), a :: X") ["+", "a"]
    -- Don't get fooled by literals.
    equal assert (f "1 :: Int") []

    -- plain functions and operators
    equal assert (f "(.::) :: X -> Y") [".::"]

test_class = do
    let f = process
    equal assert (f "class (X x) => C a b where\n\tm :: a->b\n\tn :: c\n")
        ["C", "m", "n"]
    equal assert (f "class A a where f :: X\n") ["A", "f"]
    -- indented inside where
    equal assert (f "class X where\n\ta, (+) :: X\n") ["X", "a", "+"]
    equal assert (f "class X where\n\ta :: X\n\tb, c :: Y")
        ["X", "a", "b", "c"]
    equal assert (f "class X\n\twhere\n\ta :: X\n\tb, c :: Y")
        ["X", "a", "b", "c"]
    equal assert (f "class X\n\twhere\n\ta ::\n\t\tX\n\tb :: Y")
        ["X", "a", "b"]

test_literate = do
    let f = map untag . Main.process "fn.lhs"
    equal assert (f "> class (X x) => C a b where\n>\tm :: a->b\n>\tn :: c\n")
        ["C", "m", "n"]
    equal assert (f "Test\n\\begin{code}\nclass (X x) => C a b where\n\tm :: a->b\n\tn :: c\n\\end{code}")
        ["C", "m", "n"]

process :: String -> [String]
process = map untag . Main.process "fn"

untag :: Tag -> String
untag (Right (Pos _ (Tag _ name _))) = Text.unpack name
untag (Left warn) = "warn: " ++ warn

tokenize :: Text.Text -> Main.UnstrippedTokens
tokenize = Monoid.mconcat . map Main.tokenize . Main.stripCpp
    . Main.annotate "fn"

plist :: (Show a) => [a] -> IO ()
plist xs = mapM_ (putStrLn . show) xs >> putChar '\n'

extractTokens :: Main.UnstrippedTokens -> [Text.Text]
extractTokens = map (\token -> case Main.valOf token of
    Token _ name -> name
    Newline n -> Text.pack ("nl " ++ show n)) . Main.unstrippedTokensOf

equal :: (Show a, Eq a) => Assert z -> a -> a -> IO ()
equal srcpos x y = unless (x == y) $
    putStrLn $ "__ " ++ getSourceLoc srcpos ++ " " ++ show x ++ " /= " ++ show y

type Assert a = Bool -> a -> String

-- | Awful ghc hack to get source line location.
getSourceLoc :: Assert a -> String
getSourceLoc assert_ = takeWhile (/=' ') $ Unsafe.unsafePerformIO $
    Exception.evaluate (assert_ False (error "Impossible"))
        `Exception.catch` (\(Exception.AssertionFailed s) -> return s)
