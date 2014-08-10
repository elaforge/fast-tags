{-# LANGUAGE OverloadedStrings #-}
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import Control.Monad

import qualified Data.Either as Either
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text

import Exception (assert)
import qualified System.Exit as Exit
import qualified System.IO.Unsafe as Unsafe

import qualified FastTags as FastTags
import FastTags (TokenVal(..), TagVal(..), Type(..), Tag, Pos(..))


-- | Record number of failures so I can tell the caller about it.  It would be
-- better to use a shell script and grep, but cabal test doesn't seem to
-- support that easily.
failures :: MVar.MVar Int
failures = Unsafe.unsafePerformIO (MVar.newMVar 0)
{-# NOINLINE failures #-}

-- This is kind of annoying without automatic test_* collection...
main :: IO ()
main = do
    sequence_
        [ test_tokenize, test_skipString, test_stripComments
        , test_breakBlocks, test_processAll
        , test_process
        ]
    exit =<< MVar.readMVar failures

exit :: Int -> IO a
exit 0 = Exit.exitSuccess
exit n = Exit.exitWith $ Exit.ExitFailure n

test_tokenize = do
    -- drop leading "nl 0"
    let f = drop 1 . extractTokens . tokenize
    equal assert (f "a::b->c") ["a", "::", "b", "->", "c"]
    equal assert (f "x{-\n  bc#-}\n")
        ["x", "{-", "nl 2", "bc", "#", "-}"]
    equal assert (f "X.Y") ["X.Y"]
    equal assert (f "x9") ["x9"]
    equal assert (f "x :+: y") ["x", ":+:", "y"]
    equal assert (f "(#$)") ["(#$)"]
    equal assert (f "$#-- hi") ["$#", "--", "hi"]
    equal assert (f "(*), (-)") ["(*)", ",", "(-)"]

test_skipString = do
    let f = FastTags.skipString
    equal assert (f "hi \" there") " there"
    equal assert (f "hi \\a \" there") " there"
    equal assert (f "hi \\\" there\"") ""
    equal assert (f "hi") ""
    -- String continuation isn't implemented yet.
    equal assert (f "hi \\") ""

test_stripComments = do
    let f = extractTokens . FastTags.stripComments . tokenize
    equal assert (f "hello -- there") ["nl 0", "hello"]
    equal assert (f "hello {- there -} fred") ["nl 0", "hello", "fred"]
    equal assert (f "{-# LANG #-} hello {- there {- nested -} comment -} fred")
        ["nl 0", "hello", "fred"]

test_breakBlocks = do
    let f = map
            (extractTokens . FastTags.UnstrippedTokens . FastTags.stripNewlines)
            . FastTags.breakBlocks . tokenize
    equal assert (f "1\n2\n") [["1"], ["2"]]
    equal assert (f "1\n 1\n2\n") [["1", "1"], ["2"]]
    equal assert (f "1\n 1\n 1\n2\n") [["1", "1", "1"], ["2"]]
    -- intervening blank lines are ignored
    equal assert (f "1\n 1\n\n 1\n2\n") [["1", "1", "1"], ["2"]]
    equal assert (f "1\n\n\n 1\n2\n") [["1", "1"], ["2"]]

    equal assert (f "1\n 11\n 11\n") [["1", "11", "11"]]
    equal assert (f " 11\n 11\n") [["11"], ["11"]]

test_processAll = do
    let f = map showTag . FastTags.processAll . Either.rights
            . concatMap (\(i, t) -> FastTags.process ("fn" ++ show i) t)
            . zip [0..]
        showTag (Pos p (Tag text typ)) =
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
    ]

test_misc = do
    let f text = [tag | Right (Pos _ tag) <- FastTags.process "fn" text]
    equal assert (f "module Bar.Foo where\n") [Tag "Foo" Module]
    equal assert (f "newtype Foo a b =\n\tBar x y z\n")
        [Tag "Foo" Type, Tag "Bar" Constructor]
    equal assert (f "f :: A -> B\ng :: C -> D\ndata D = C {\n\tf :: A\n\t}\n")
        [Tag "f" Function, Tag "g" Function, Tag "D" Type,
            Tag "C" Constructor, Tag "f" Function]

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
    -- Not confused by a class context on a method.
    equal assert (f "class X a where\n\tfoo :: Eq a => a -> a\n")
        ["X", "foo"]

process :: Text.Text -> [String]
process = map untag . FastTags.process "fn"

untag :: Tag -> String
untag (Right (Pos _ (Tag name _))) = Text.unpack name
untag (Left warn) = "warn: " ++ warn

tokenize :: Text.Text -> FastTags.UnstrippedTokens
tokenize = Monoid.mconcat . map FastTags.tokenize . FastTags.stripCpp
    . FastTags.annotate "fn"

plist :: (Show a) => [a] -> IO ()
plist xs = mapM_ (putStrLn . show) xs >> putChar '\n'

extractTokens :: FastTags.UnstrippedTokens -> [Text.Text]
extractTokens = map (\token -> case FastTags.valOf token of
    Token name -> name
    Newline n -> Text.pack ("nl " ++ show n)) . FastTags.unstrippedTokensOf

equal :: (Show a, Eq a) => Assert z -> a -> a -> IO ()
equal srcpos x y = unless (x == y) $ do
    putStrLn $ "__ " ++ getSourceLoc srcpos ++ " " ++ show x ++ " /= " ++ show y
    MVar.modifyMVar_ failures (return . (+1))

type Assert a = Bool -> a -> String

-- | Awful ghc hack to get source line location.
getSourceLoc :: Assert a -> String
getSourceLoc assert_ = takeWhile (/=' ') $ Unsafe.unsafePerformIO $
    Exception.evaluate (assert_ False (error "srcloc hack failed"))
        `Exception.catch` (\(Exception.AssertionFailed s) -> return s)
