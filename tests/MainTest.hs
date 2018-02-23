{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow (first)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T

import qualified Test.Tasty as Tasty
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.HUnit as HUnit

import qualified FastTags.Lexer as Lexer
import qualified FastTags.Tag as Tag
import FastTags.Tag hiding (process)
import FastTags.Token
import qualified FastTags.Vim as Vim


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = testGroup "tests"
    [ testTokenize
    , testTokenizeWithNewlines
    , testStripComments
    , testBreakBlocks
    , testVim
    , testProcess
    , testStripCpp
    ]

class ExtendedEq a where
    (===) :: a -> a -> Bool
    default (===) :: Eq a => a -> a -> Bool
    (===) = (==)

instance (ExtendedEq a) => ExtendedEq [a] where
    (===) xs ys = length xs == length ys && and (zipWith (===) xs ys)

instance (ExtendedEq a, ExtendedEq b) => ExtendedEq (a, b) where
    (===) (a, b) (c, d) = a === c && b === d

instance ExtendedEq TagVal where
    TagVal name t parent === TagVal name' t' parent' =
        name == name' && t == t' && parent == parent'

instance (ExtendedEq a) => ExtendedEq (Pos a) where
    (===) (Pos x y) (Pos x' y') = x === x' && y === y'

instance ExtendedEq a => ExtendedEq (Maybe a) where
    (===) Nothing  Nothing  = True
    (===) (Just x) (Just y) = x === y
    (===) _        _        = False

instance (ExtendedEq a, ExtendedEq b, ExtendedEq c) => ExtendedEq (a, b, c) where
    (a, b, c) === (a', b', c') = a === a' && b === b' && c === c'

instance ExtendedEq Int
instance ExtendedEq Char
instance ExtendedEq Text
instance ExtendedEq TokenVal
instance ExtendedEq SrcPos
instance ExtendedEq Vim.Parsed

testTokenize :: TestTree
testTokenize = testGroup "tokenize"
    [ "xyz  -- abc"       ==> [T "xyz", Newline 0]
    , "xyz  --- abc"      ==> [T "xyz", Newline 0]
    , "  {-   foo -}"     ==> [Newline 0]
    , "  {-   foo \n\
      \\n\
      \-}"                ==> [Newline 0]
    , "  {- foo {- bar-} -}" ==> [Newline 0]
    , "  {-# INLINE #-}"  ==> [Newline 0]
    , "a::b->c"           ==>
        [T "a", DoubleColon, T "b", Arrow, T "c", Newline 0]
    , "a∷b→c"             ==>
        [T "a", DoubleColon, T "b", Arrow, T "c", Newline 0]
    , "x{-\n  bc#-}\n"    ==> [T "x", Newline 0]
    , "X.Y"               ==> [T "X.Y", Newline 0]
    , "a=>b"              ==> [T "a", Implies, T "b", Newline 0]
    , "a ⇒ b"             ==> [T "a", Implies, T "b", Newline 0]
    , "x9"                ==> [T "x9", Newline 0]
    -- , "9x" ==> ["nl 0", "9", "x"]
    , "x :+: y"           ==> [T "x", T ":+:", T "y", Newline 0]
    , "(#$)"              ==> [LParen, T "#$", RParen, Newline 0]
    , "Data.Map.map"      ==> [T "Data.Map.map", Newline 0]
    , "Map.map"           ==> [T "Map.map", Newline 0]
    , "forall a. f a"     ==> [T "forall", T "a", Dot, T "f", T "a", Newline 0]
    , "forall a . Foo"    ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
    , "forall a. Foo"     ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
    , "forall a .Foo"     ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
    , "forall a.Foo"      ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
    , "$#-- hi"           ==> [T "$#--", T "hi", Newline 0]
    , "(*), (-)"          ==>
        [LParen, T "*", RParen, Comma, LParen, T "-", RParen, Newline 0]
    , "(.::)"             ==> [LParen, T ".::", RParen, Newline 0]
    -- we rely on this behavior
    , "data (:+) a b"     ==>
        [KWData, LParen, T ":+", RParen, T "a", T "b", Newline 0]
    , "data (.::+::) a b" ==>
        [KWData, LParen, T ".::+::", RParen, T "a", T "b", Newline 0]
    -- string tokenization
    , "foo \"bar\" baz"   ==> [T "foo", String, T "baz", Newline 0]
    -- multiline string
    , "foo \"bar\\\n\
      \  \\bar\" baz"     ==> [T "foo", String, T "baz", Newline 0]
    , "(\\err -> case err of Foo -> True; _ -> False)" ==>
        [ LParen, T "\\", T "err", Arrow, KWCase, T "err", KWOf, T "Foo"
        , Arrow, T "True", T "_", Arrow, T "False", RParen, Newline 0
        ]
    , "foo = \"foo\\n\\\n\
      \  \\\" bar" ==> [T "foo", Equals, String, T "bar", Newline 0]
    , "foo = \"foo\\n\\\n\
      \  \\x\" bar" ==>
        [ T "foo", Equals, String, T "bar", Newline 0]
    , tokenizeSplices
    ]
    where
    (==>) = test f
    f :: Text -> [TokenVal]
    f = tail -- strip uninteresting initial newline
      . map valOf
      . unstrippedTokensOf
      . tokenize

    tokenizeSplices = testGroup "tokenize splices"
        [ "$(foo)"                              ==>
            [SpliceStart, T "foo", RParen, Newline 0]
        , "$(foo [| baz |])"                    ==>
            [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
            , Newline 0
            ]
        , "$(foo [bar| baz |])"                 ==>
            [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
            , Newline 0
            ]
        , "$(foo [bar| bar!\nbaz!\n $(baz) |])" ==>
             [ SpliceStart, T "foo", QuasiquoterStart, SpliceStart, T "baz"
             , RParen, QuasiquoterEnd, RParen, Newline 0
             ]
        ]

testTokenizeWithNewlines :: TestTree
testTokenizeWithNewlines = testGroup "tokenize with newlines"
    [ "1\n2\n"     ==> [Newline 0, T "1", Newline 0, T "2", Newline 0]
    , " 11\n 11\n" ==> [Newline 1, T "11", Newline 1, T "11", Newline 0]
    ]
    where
    (==>) = test f
    f = map valOf . unstrippedTokensOf . tokenize

testStripComments :: TestTree
testStripComments = testGroup "stripComments"
    [ "hello -- there"                                           ==>
        ["nl 0", "hello", "nl 0"]
    , "hello --there"                                            ==>
        ["nl 0", "hello", "nl 0"]
    , "hello {- there -} fred"                                   ==>
        ["nl 0", "hello", "fred", "nl 0"]
    , "hello -- {- there -}\nfred"                               ==>
        ["nl 0", "hello", "nl 0", "fred", "nl 0"]
    , "{-# LANG #-} hello {- there {- nested -} comment -} fred" ==>
        ["nl 0", "hello", "fred", "nl 0"]
    , "hello {-\nthere\n------}\n fred"                          ==>
        ["nl 0", "hello",  "nl 1", "fred", "nl 0"]
    , "hello {-  \nthere\n  ------}  \n fred"                    ==>
        ["nl 0", "hello",  "nl 1", "fred", "nl 0"]
    , "hello {-\nthere\n-----}\n fred"                           ==>
        ["nl 0", "hello", "nl 1", "fred", "nl 0"]
    , "hello {-  \nthere\n  -----}  \n fred"                     ==>
        ["nl 0", "hello",  "nl 1", "fred", "nl 0"]
    , "hello {-\n-- there -}"                                    ==>
        ["nl 0", "hello", "nl 0"]
    , "foo --- my comment\n--- my other comment\nbar"            ==>
        ["nl 0", "foo", "nl 0", "nl 0", "bar", "nl 0"]
    ]
    where
    (==>) = test f
    f = extractTokens . tokenize

testBreakBlocks :: TestTree
testBreakBlocks = testGroup "breakBlocks"
    [ "1\n2\n"           ==> [["1"], ["2"]]
    , "1\n 1\n2\n"       ==> [["1", "1"], ["2"]]
    , "1\n 1\n 1\n2\n"   ==> [["1", "1", "1"], ["2"]]
    -- intervening blank lines are ignored
    , "1\n 1\n\n 1\n2\n" ==> [["1", "1", "1"], ["2"]]
    , "1\n\n\n 1\n2\n"   ==> [["1", "1"], ["2"]]

    , "1\n 11\n 11\n"    ==> [["1", "11", "11"]]
    , " 11\n 11\n"       ==> [["11"], ["11"]]
    ]
    where
    (==>) = test f
    f = map (extractTokens . UnstrippedTokens . Tag.stripNewlines)
      . Tag.breakBlocks
      . tokenize

testVim :: TestTree
testVim = testGroup "Vim" [parseTag, dropAdjacent]

parseTag :: TestTree
parseTag = testGroup "parseTag"
    [ ("fn", 1, "text", Tag.Function) ==>
        Just (Vim.Parsed "text" Tag.Function "fn" 1)
    ]
    where
    (==>) = test f
    f (fn, line, text, typ) =
        Vim.parseTag $ Vim.showTag (Pos (SrcPos fn line "") (TagVal text typ Nothing))

dropAdjacent :: TestTree
dropAdjacent = testGroup "dropAdjacent"
    [ [fna 0] ==> [fna 0]
    , [fna 0, fna 1] ==> [fna 0]
    -- Line number order dosen't matter.
    , [fna 1, fna 0] ==> [fna 0]
    , [fna 0, fna 1, fna 2] ==> [fna 0]
    , [fna 0, fna 1, fna 2, fna 3] ==> [fna 0, fna 3]

    -- Filename differs.
    , [fna 0, ("fn2", 1, "a")] ==> [fna 0, ("fn2", 1, "a")]
    -- Tag name differs.
    , [fna 0, ("fn", 1, "b")] ==> [fna 0, ("fn", 1, "b")]
    ]
    where
    fna n = ("fn", n, "a")
    (==>) = test f
    f = map extract . map fst . Vim.dropAdjacent 2
        . Vim.keyOnJust Vim.parseTag . map (Vim.showTag . makeTag)
    extract t = (Vim.filename t, Vim.line t, Vim.name t)
    makeTag (fn, line, text) = Pos (SrcPos fn line "") (TagVal text Function Nothing)

testProcess :: TestTree
testProcess = testGroup "process"
    [ testPrefixes
    , testData
    , testGADT
    , testFamilies
    , testFunctions
    , testClass
    , testInstance
    , testLiterate
    , testPatterns
    , testFFI
    ]

testPrefixes :: TestTree
testPrefixes = testGroup "prefix tracking"
    [ "module Bar.Foo where\n" ==>
        [Pos (SrcPos fn 1 "module Bar.Foo") (TagVal "Foo" Module Nothing)]
    , "newtype Foo a b =\n\
      \\tBar x y z\n" ==>
        [ Pos (SrcPos fn 1 "newtype Foo") (TagVal "Foo" Type Nothing)
        , Pos (SrcPos fn 2 "\tBar") (TagVal "Bar" Constructor (Just "Foo"))
        ]
    , "data Foo a b =\n\
      \\tBar x y z\n" ==>
        [ Pos (SrcPos fn 1 "data Foo") (TagVal "Foo" Type Nothing)
        , Pos (SrcPos fn 2 "\tBar") (TagVal "Bar" Constructor (Just "Foo"))
        ]
    , "f :: A -> B\n\
      \g :: C -> D\n\
      \data D = C {\n\
      \\tf :: A\n\
      \\t}\n" ==>
        [ Pos (SrcPos fn 1 "f") (TagVal "f" Function Nothing)
        , Pos (SrcPos fn 2 "g") (TagVal "g" Function Nothing)
        , Pos (SrcPos fn 3 "data D") (TagVal "D" Type Nothing)
        , Pos (SrcPos fn 3 "data D = C") (TagVal "C" Constructor (Just "D"))
        , Pos (SrcPos fn 4 "\tf") (TagVal "f" Function (Just "D"))
        ]
    , "instance Foo Bar where\n\
      \  newtype FooFam Bar = BarList [Int]" ==>
        [ Pos (SrcPos fn 2 "  newtype FooFam Bar = BarList")
              (TagVal "BarList" Constructor (Just "FooFam"))
        ]
    , "instance Foo Bar where\n\
      \  newtype FooFam Bar = BarList { getBarList :: [Int] }" ==>
        [ Pos (SrcPos fn 2 "  newtype FooFam Bar = BarList" )
              (TagVal "BarList" Constructor (Just "FooFam"))
        , Pos (SrcPos fn 2 "  newtype FooFam Bar = BarList { getBarList" )
              (TagVal "getBarList" Function (Just "FooFam"))
        ]
    , "instance Foo Bar where\n\
      \  data (Ord a) => FooFam Bar a = BarList { getBarList :: [a] }\n\
      \                               | BarMap { getBarMap :: Map a Int }" ==>
        [ Pos (SrcPos fn 2 "  data (Ord a) => FooFam Bar a = BarList")
              (TagVal "BarList" Constructor (Just "FooFam"))
        , Pos (SrcPos fn 2 "  data (Ord a) => FooFam Bar a = BarList { getBarList")
              (TagVal "getBarList" Function (Just "FooFam"))
        , Pos (SrcPos fn 3 "                               | BarMap")
              (TagVal "BarMap" Constructor (Just "FooFam"))
        , Pos (SrcPos fn 3"                               | BarMap { getBarMap")
              (TagVal "getBarMap" Function (Just "FooFam"))
        ]
    , "newtype instance FooFam Bar = BarList { getBarList :: [Int] }" ==>
        [ Pos (SrcPos fn 1 "newtype instance FooFam Bar = BarList")
              (TagVal "BarList" Constructor (Just "FooFam"))
        , Pos (SrcPos fn 1 "newtype instance FooFam Bar = BarList { getBarList")
              (TagVal "getBarList" Function (Just "FooFam"))
        ]
    , "data instance (Ord a) => FooFam Bar a = BarList { getBarList :: [a] }\n\
      \                                      | BarMap { getBarMap :: Map a Int }"
      ==>
      [ Pos (SrcPos fn 1 "data instance (Ord a) => FooFam Bar a = BarList")
            (TagVal "BarList" Constructor (Just "FooFam"))
      , Pos (SrcPos fn 1 "data instance (Ord a) => FooFam Bar a = BarList { getBarList")
            (TagVal "getBarList" Function (Just "FooFam"))
      , Pos (SrcPos fn 2 "                                      | BarMap")
            (TagVal "BarMap" Constructor (Just "FooFam"))
      , Pos (SrcPos fn 2 "                                      | BarMap { getBarMap")
            (TagVal "getBarMap" Function (Just "FooFam"))
      ]
    ]
    where
    (==>) = testFullTagsWithPrefixes fn
    fn = "fn.hs"

testData :: TestTree
testData = testGroup "data"
    [ "data X\n"                            ==> ["X"]
    , "data X = X Int\n"                    ==> ["X", "X"]
    , "data Foo = Bar | Baz"                ==> ["Bar", "Baz", "Foo"]
    , "data Foo =\n\tBar\n\t| Baz"          ==> ["Bar", "Baz", "Foo"]
    -- Records.
    , "data Foo a = Bar { field :: Field }" ==> ["Bar", "Foo", "field"]
    , "data R = R { a::X, b::Y }"           ==> ["R", "R", "a", "b"]
    , "data R = R { a∷X, b∷Y }"             ==> ["R", "R", "a", "b"]
    , "data R = R {\n\ta::X\n\t, b::Y\n\t}" ==> ["R", "R", "a", "b"]
    , "data R = R {\n\ta,b::X\n\t}"         ==> ["R", "R", "a", "b"]
    -- Record operators
    , "data Foo a b = (:*:) { foo :: a, bar :: b }" ==>
        [":*:", "Foo", "bar", "foo"]

    , "data R = R {\n\
      \    a :: !RealTime\n\
      \  , b :: !RealTime\n\
      \}"
      ==>
      ["R", "R", "a", "b"]
    , "data Rec = Rec {\n\
      \  a :: Int\n\
      \, b :: !Double\n\
      \, c :: Maybe Rec\
      \\n\
      \}"
      ==>
      ["Rec", "Rec", "a", "b", "c"]

    , "data X = X !Int"                            ==> ["X", "X"]
    , "data X = Y !Int !X | Z"                     ==> ["X", "Y", "Z"]
    , "data X = Y :+: !Z | !X `Mult` X"            ==> [":+:", "Mult", "X"]
    , "data X = !Y `Add` !Z"                       ==> ["Add", "X"]

    , "data X = forall a. Y a"                     ==> ["X", "Y"]
    , "data X = forall a . Y a"                    ==> ["X", "Y"]
    , "data X = forall a .Y a"                     ==> ["X", "Y"]
    , "data X = forall a.Y a"                      ==> ["X", "Y"]
    , "data X = forall a. Eq a => Y a"             ==> ["X", "Y"]
    , "data X = forall a. (Eq a) => Y a"           ==> ["X", "Y"]
    , "data X = forall (a :: Nat). (Eq' a) => Y a" ==> ["X", "Y"]
    , "data X = forall a. (Eq a, Ord a) => Y a"    ==> ["X", "Y"]
    , "data X = forall a. Ref :<: a => Y a"        ==> ["X", "Y"]
    , "data X = forall a. (:<:) Ref a => Y a"      ==> ["X", "Y"]
    , "data X = forall a. ((:<:) Ref a) => Y a"    ==> ["X", "Y"]
    , "data X = forall a. Y !a"                    ==> ["X", "Y"]
    , "data X = forall a. (Eq a, Ord a) => Y !a"   ==> ["X", "Y"]

    , "data Foo a = \n\
      \    Plain Int\n\
      \  | forall a. Bar a Int\n\
      \  | forall a b. Baz b a\n\
      \  | forall a . Quux a \
      \  | forall a .Quuz a"
      ==>
      ["Bar", "Baz", "Foo", "Plain", "Quux", "Quuz"]

    , "data X a = Add a "                     ==> ["Add", "X"]
    , "data Eq a => X a = Add a"              ==> ["Add", "X"]
    , "data (Eq a) => X a = Add a"            ==> ["Add", "X"]
    , "data (Eq a, Ord a) => X a = Add a"     ==> ["Add", "X"]
    , "data (Eq (a), Ord (a)) => X a = Add a" ==> ["Add", "X"]

    , "data Ref :<: f => X f = RRef f"                                  ==>
        ["RRef", "X"]
    , "data a :<: b => X a b = Add a"                                   ==>
        ["Add", "X"]
    , "newtype Ref :<: f => X f = RRef f"                               ==>
        ["RRef", "X"]
    , "newtype a :<: b => X a b = Add a"                                ==>
        ["Add", "X"]
    , "data Ref :<: [f] => X f = RRef f"                                ==>
        ["RRef", "X"]
    , "data [a] :<: b => X a b = Add a"                                 ==>
        ["Add", "X"]
    , "newtype Ref :<: [f] => X f = RRef f"                             ==>
        ["RRef", "X"]
    , "newtype [a] :<: b => X a b = Add a"                              ==>
        ["Add", "X"]

    , "data (a :<: b) => X a b = Add a"                                 ==>
        ["Add", "X"]
    , "data a :><: b = a :>|<: b"                                       ==>
        [":><:", ":>|<:"]
    , "data (:><:) a b = (:>|<:) a b"                                   ==>
        [":><:", ":>|<:"]
    , "data (:><:) a b = Foo b | (:>|<:) a b"                           ==>
        [":><:", ":>|<:", "Foo"]
    , "data (:><:) a b = Foo b | forall c. (:>|<:) a c"                 ==>
        [":><:", ":>|<:", "Foo"]
    , "data (:><:) a b = Foo b | forall c. (Eq c) => (:>|<:) a c"       ==>
        [":><:", ":>|<:", "Foo"]
    , "data (:><:) a b = Foo b | forall c. Eq c => (:>|<:) a c"         ==>
        [":><:", ":>|<:", "Foo"]

    , "newtype Eq a => X a = Add a"                                     ==>
        ["Add", "X"]
    , "newtype (Eq a) => X a = Add a"                                   ==>
        ["Add", "X"]
    , "newtype (Eq a, Ord a) => X a = Add a"                            ==>
        ["Add", "X"]

    , "newtype (u :*: v) z = X"                                         ==>
        [":*:", "X"]
    , "data (u :*: v) z = X"                                            ==>
        [":*:", "X"]
    , "type (u :*: v) z = (u, v, z)"                                    ==>
        [":*:"]

    , "newtype ((u :: (* -> *) -> *) :*: v) z = X"                      ==>
        [":*:", "X"]
    , "data ((u :: (* -> *) -> *) :*: v) z = X"                         ==>
        [":*:", "X"]
    , "type ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"                 ==>
        [":*:"]

    , "newtype (Eq (v z)) => ((u :: (* -> *) -> *) :*: v) z = X"        ==>
        [":*:", "X"]
    , "data (Eq (v z)) => ((u :: (* -> *) -> *) :*: v) z = X"           ==>
        [":*:", "X"]
    , "type (Eq (v z)) => ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"   ==>
        [":*:"]

    , "newtype Eq (v z) => ((u :: (* -> *) -> *) :*: v) z = X"          ==>
        [":*:", "X"]
    , "data Eq (v z) => ((u :: (* -> *) -> *) :*: v) z = X"             ==>
        [":*:", "X"]
    , "type Eq (v z) => ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"     ==>
        [":*:"]

    , "newtype (Eq (v z)) => ((u :: (* -> *) -> *) `Foo` v) z = X"      ==>
        ["Foo", "X"]
    , "data (Eq (v z)) => ((u :: (* -> *) -> *) `Foo` v) z = X"         ==>
        ["Foo", "X"]
    , "type (Eq (v z)) => ((u :: (* -> *) -> *) `Foo` v) z = (u, v, z)" ==>
        ["Foo"]
    , "type (Eq (v z)) => ((u ∷ (* -> *) -> *) `Foo` v) z = (u, v, z)"  ==>
        ["Foo"]

    , "newtype Eq (v z) => ((u :: (* -> *) -> *) `Foo` v) z = X"        ==>
        ["Foo", "X"]
    , "data Eq (v z) => ((u :: (* -> *) -> *) `Foo` v) z = X"           ==>
        ["Foo", "X"]
    , "type Eq (v z) => ((u :: (* -> *) -> *) `Foo` v) z = (u, v, z)"   ==>
        ["Foo"]
    , "type Eq (v z) ⇒ ((u ∷ (* → *) → *) `Foo` v) z = (u, v, z)"       ==>
        ["Foo"]

    , "data (:*:) u v z = X"                                            ==>
        [":*:", "X"]
    , "data (Eq (u v), Ord (z)) => (:*:) u v z = X"                     ==>
        [":*:", "X"]
    , "data (u `W` v) z = X"                                            ==>
        ["W", "X"]
    , "data (Eq (u v), Ord (z)) => (u `W` v) z = X"                     ==>
        ["W", "X"]

    , "newtype X a = Z {\n\
      \ -- TODO blah\n\
      \ foo :: [a] }"
      ==>
      ["X", "Z", "foo"]
    , "newtype (u :*: v) z = X {\n\
      \ -- my insightful comment\n\
      \ extract :: (u (v z)) }"
      ==>
      [":*:", "X", "extract"]
    , "newtype (u :*: v) z = X {\n\
      \ -- my insightful comment\n\
      \ pattern :: (u (v z)) }"
      ==>
      [":*:", "X", "pattern"]

    , "data Hadron a b = Science { f :: a, g :: a, h :: b }"
      ==>
      ["Hadron", "Science", "f", "g", "h"]
    , "data Hadron a b = Science { f :: a, g :: a, pattern :: b }"
      ==>
      ["Hadron", "Science", "f", "g", "pattern"]
    , "data Hadron a b = Science { f :: a, g :: (a, b), h :: b }"
      ==>
      ["Hadron", "Science", "f", "g", "h"]
    , "data Hadron a b = forall x. Science { f :: x, h :: b }"
      ==>
      ["Hadron", "Science", "f", "h"]
    , "data Hadron a b = forall x. Science { f :: [x], h :: b }"
      ==>
      ["Hadron", "Science", "f", "h"]
    , "data Hadron a b = forall x y. Science { f :: (x, y), h :: b }"
      ==>
      ["Hadron", "Science", "f", "h"]
    , "data Hadron a b = forall x y. Science { f :: [(x, y)], h :: b }"
      ==>
      ["Hadron", "Science", "f", "h"]
    , "data Hadron a b = \
      \forall x y. Science { f :: [(Box x, Map x y)], h :: b }"
      ==>
      ["Hadron", "Science", "f", "h"]
    , "data Hadron a b = forall x y. Science { f ∷ [(Box x, Map x y)], h ∷ b }"
      ==>
      ["Hadron", "Science", "f", "h"]
    , "data Hadron a b = forall x y z. Science\n\
      \  { f :: x\n\
      \  , g :: [(Box x, Map x y, z)] \
      \  }"
      ==>
      ["Hadron", "Science", "f", "g"]
    , "data Hadron a b = forall x y z. Science\n\
      \  { f :: x\n\
      \  , g :: [(Box x, Map x y, z)] \
      \  , h :: b\n\
      \  }"
      ==>
      ["Hadron", "Science", "f", "g", "h"]
    , "data Hadron a b = Science { h :: b }"
      ==>
      ["Hadron", "Science", "h"]
    , "data Test a b =\n\
      \    Foo a\n\
      \  | Bar [(Maybe a, Map a b, b)]"
      ==>
      ["Bar", "Foo", "Test"]
    , "data Test a b =\n\
      \    Foo a\n\
      \  | [(Maybe b, Map b a, a)] `Bar` [(Maybe a, Map a b, b)]"
      ==>
      ["Bar", "Foo", "Test"]
    ]
    where
    (==>) = testTagNames filename

testGADT :: TestTree
testGADT = testGroup "gadt"
    [ "data X where A :: X\n"              ==> ["A", "X"]
    , "data X where\n\tA :: X\n"           ==> ["A", "X"]
    , "data X where\n\tA :: X\n\tB :: X\n" ==> ["A", "B", "X"]
    , "data X where\n\tA, B :: X\n"        ==> ["A", "B", "X"]
    , "data X :: * -> * -> * where\n\
      \  A, B :: Int -> Int -> X\n"
      ==>
      ["A", "B", "X"]
    , "data X ∷ * → * → * where\n\
      \  A, B ∷ Int → Int → X\n"
      ==>
      ["A", "B", "X"]
    , "data Vec ix where\n\
      \  Nil   :: Int -> Foo Int\n\
      \  (:::) :: Int -> Vec Int -> Vec Int\n\
      \  (.+.) :: Int -> Int -> Vec Int -> Vec Int\n"
      ==>
      [".+.", ":::", "Nil", "Vec"]
    , "data Vec ix where\n\
      \  Nil   :: Int -> Foo Int\n\
      \  -- foo\n\
      \  (:::) :: Int -> Vec Int -> Vec Int\n\
      \-- bar\n\
      \  (.+.) :: Int     -> \n\
      \           -- ^ baz\n\
      \           Int     -> \n\
      \           Vec Int -> \n\
      \Vec Int\n"
      ==>
      [".+.", ":::", "Nil", "Vec"]
    , "data NatSing (n :: Nat) where\n\
      \  ZeroSing :: 'Zero\n\
      \  SuccSing :: NatSing n -> NatSing ('Succ n)\n"
      ==>
      ["NatSing", "SuccSing", "ZeroSing"]
    , "data Rec a where\n\
      \  C :: { foo :: Int } -> Rec a"
      ==> ["C", "Rec", "foo"]
    , "data Rec a where\n\
      \  C :: { foo :: Int, bar :: Int -> Int } -> Rec a"
      ==> ["C", "Rec", "bar", "foo"]
    , "data Rec a where\n\
      \  C :: { foo :: Int, bar :: Int -> Int } -> Rec a\n\
      \  D :: { baz :: (Int -> Int) -> Int, bar :: (((Int) -> (Int))) } -> Rec a"
      ==> ["C", "D", "Rec", "bar", "bar", "baz", "foo"]
    ]
    where
    (==>) = testTagNames filename

testFamilies :: TestTree
testFamilies = testGroup "families"
    [ "type family X :: *\n"      ==> ["X"]
    , "data family X :: * -> *\n" ==> ["X"]
    , "data family a ** b"        ==> ["**"]

    , "type family a :<: b\n"                               ==> [":<:"]
    , "type family (a :: Nat) :<: (b :: Nat) :: Nat\n"      ==> [":<:"]
    , "type family (a :: Nat) `Family` (b :: Nat) :: Nat\n" ==> ["Family"]
    , "type family (m :: Nat) <=? (n :: Nat) :: Bool"       ==> ["<=?"]
    , "type family (m ∷ Nat) <=? (n ∷ Nat) ∷ Bool"         ==> ["<=?"]

    , "data instance X a b = Y a | Z { unZ :: b }"                     ==>
        ["Y", "Z", "unZ"]
    , "data instance (Eq a, Eq b) => X a b = Y a | Z { unZ :: b }"     ==>
        ["Y", "Z", "unZ"]
    , "data instance (Eq a, Eq b) => X a b = Y a | Z { pattern :: b }" ==>
        ["Y", "Z", "pattern"]
    , "data instance XList Char = XCons !Char !(XList Char) | XNil"    ==>
        ["XCons", "XNil"]
    , "newtype instance Cxt x => T [x] = A (B x) deriving (Z,W)"       ==> ["A"]
    , "type instance Cxt x => T [x] = A (B x)"                         ==> []
    , "data instance G [a] b where\n\
      \   G1 :: c -> G [Int] b\n\
      \   G2 :: G [a] Bool"
      ==>
      ["G1", "G2"]
    , "class C where\n\ttype X y :: *\n" ==> ["C", "X"]
    , "class C where\n\tdata X y :: *\n" ==> ["C", "X"]
    , "class C where\n\ttype X y ∷ *\n"  ==> ["C", "X"]
    , "class C where\n\tdata X y ∷ *\n"  ==> ["C", "X"]
    ]
    where
    (==>) = testTagNames filename

testFunctions :: TestTree
testFunctions = testGroup "functions"
    -- Multiple declarations.
    [ "a,b::X"          ==> ["a", "b"]
    -- With an operator.
    , "(+), a :: X"     ==> ["+", "a"]
    -- Unicode operator.
    , "(•) :: X"        ==> ["•"]
    -- Don't get fooled by literals.
    , "1 :: Int"        ==> []
    -- Don't confuse _ wildcard for an operator.
    , "f :: Int -> Int\n\
      \f _ = 1"         ==> ["f"]

    -- plain functions and operators
    , "(.::) :: X -> Y" ==> [".::"]
    , "(:::) :: X -> Y" ==> [":::"]
    , "(->:) :: X -> Y" ==> ["->:"]
    , "(--+) :: X -> Y" ==> ["--+"]
    , "(=>>) :: X -> Y" ==> ["=>>"]

    , "assertDataFormatError :: DecompressError -> IO String\n\
      \assertDataFormatError (DataFormatError detail) = return detail\n\
      \assertDataFormatError _                        = assertFailure \"expected DataError\"\n\
      \                                              >> return \"\"\n"
      ==>
      ["assertDataFormatError"]

    , "(r `mkQ` br) a = _" ==> ["mkQ"]

    , "_g :: X -> Y"    ==> ["_g"]
    , toplevelFunctionsWithoutSignatures
    ]
    where
    (==>) = testTagNames filename
    toplevelFunctionsWithoutSignatures =
        testGroup "toplevel functions without signatures"
        [ "infix 5 |+|"  ==> []
        , "infixl 5 |+|" ==> []
        , "infixr 5 |+|" ==> []
        , "f = g"        ==> ["f"]
        -- Relies on RepeatableTag.
        , "f :: a -> b -> a\n\
          \f x y = x"
          ==>
          ["f"]
        , "f x y = x"               ==> ["f"]
        , "f (x :+: y) z = x"       ==> ["f"]
        , "(x :+: y) `f` z = x"     ==> ["f"]
        , "(x :+: y) :*: z = x"     ==> [":*:"]
        , "((:+:) x y) :*: z = x"   ==> [":*:"]
        , "(:*:) (x :+: y) z = x"   ==> [":*:"]
        , "(:*:) ((:+:) x y) z = x" ==> [":*:"]
        , strictMatchTests
        , lazyMatchTests
        , atPatternsTests
        , "f x Nothing = x\n\
          \f x (Just y) = y"
          ==>
          ["f"]
        , "x `f` Nothing = x\n\
          \x `f` (Just y) = y"
          ==>
          ["f"]
        , "f x y = g x\n\
          \  where\n\
          \    g _ = y"
          ==>
          ["f"]
        , "x `f` y = x"   ==> ["f"]
        , "(|+|) x y = x" ==> ["|+|"]
        , "x |+| y = x"   ==> ["|+|"]
        , "(!) x y = x" ==> ["!"]
        , "--- my comment" ==> []
        , "foo :: Rec -> Bar\n\
          \foo Rec{..} = Bar (recField + 1)"
          ==>
          ["foo"]
        , "foo :: Rec -> Bar\n\
          \foo Rec { bar = Baz {..}} = Bar (recField + 1)"
          ==>
          ["foo"]
        -- Functions named "pattern"
        , "pattern :: Int -> String -> [Int]"           ==> ["pattern"]
        , "pattern, foo :: Int -> String -> [Int]"      ==> ["foo", "pattern"]
        , "foo, pattern :: Int -> String -> [Int]"      ==> ["foo", "pattern"]
        , "foo, pattern, bar :: Int -> String -> [Int]" ==> ["bar", "foo", "pattern"]
        , "pattern x = x "                              ==> ["pattern"]
        , "pattern x y = x + y"                         ==> ["pattern"]
        , "x `pattern` y = x + y"                       ==> ["pattern"]
        , "pattern x y z = x + y + z"                   ==> ["pattern"]
        -- Arguments named "forall
        , "f forall = forall + 1"                       ==> ["f"]
        ]
    strictMatchTests = testGroup "strict match (!)"
        [ "f !x y = x"                ==> ["f"]
        , "f x !y = x"                ==> ["f"]
        , "f !x !y = x"               ==> ["f"]
        , "f ! x y = x"               ==> ["f"]
        -- this one is a bit controversial but it seems to be the way ghc
        -- parses it
        , "f ! x = x"                 ==> ["f"]
        , "(:*:) !(x :+: y) z = x"    ==> [":*:"]
        , "(:*:) !(!x :+: !y) !z = x" ==> [":*:"]
        , "(:*:) !((:+:) x y) z = x"  ==> [":*:"]
        , "(:*:) !((:+:) !x !y) !z = x" ==> [":*:"]
        , "(!) :: a -> b -> a\n\
          \(!) x y = x"
          ==>
          ["!"]
        -- this is a degenerate case since even ghc treats ! here as
        -- a BangPatterns instead of operator
        , "x ! y = x" ==> ["x"]
        ]
    lazyMatchTests = testGroup "lazy match (~)"
        [ "f ~x y = x"  ==> ["f"]
        , "f x ~y = x"  ==> ["f"]
        , "f ~x ~y = x" ==> ["f"]
        , "f ~ x y = x" ==> ["f"]
        -- this one is a bit controversial but it seems to be the way ghc
        -- parses it
        , "f ~ x = x"   ==> ["f"]
        , "(:*:) ~(x :+: y) z = x" ==> [":*:"]
        , "(:*:) ~(~x :+: ~y) ~z = x" ==> [":*:"]
        , "(:*:) ~((:+:) x y) z = x" ==> [":*:"]
        , "(:*:) ~((:+:) ~x ~y) ~z = x" ==> [":*:"]
        , "(~) :: a -> b -> a\n\
          \(~) x y = x"
          ==>
          ["~"]
        -- this is a degenerate case since even ghc treats ~ here as
        -- a BangPatterns instead of operator
        , "x ~ y = x" ==> ["x"]
        ]
    atPatternsTests = testGroup "at patterns (@)"
        [ "f z@x y    = z"                        ==> ["f"]
        , "f x   z'@y = z'"                       ==> ["f"]
        , "f z@x z'@y = z"                        ==> ["f"]
        , "f z@(Foo _) z'@y = z"                  ==> ["f"]
        , "f z@(Foo _) z'@(Bar _) = z"            ==> ["f"]
        , "f z @ x y  = z"                        ==> ["f"]
        , "f z @ (x : xs) = z: [x: xs]"           ==> ["f"]
        , "f z @ (x : zs @ xs) = z: [x: zs]"      ==> ["f"]
        , "f z @ (zz @x : zs @ xs) = z: [zz: zs]" ==> ["f"]

        , "(:*:) zzz@(x :+: y) z = x"             ==> [":*:"]
        , "(:*:) zzz@(zx@x :+: zy@y) zz@z = x"    ==> [":*:"]
        , "(:*:) zzz@((:+:) x y) z = x"           ==> [":*:"]
        , "(:*:) zzz@((:+:) zs@x zs@y) zz@z = x"  ==> [":*:"]

        , "f z@(!x) ~y = x"                       ==> ["f"]
        ]

testClass :: TestTree
testClass = testGroup "class"
    [ "class (X x) => C a b where\n\tm :: a->b\n\tn :: c\n"          ==>
        ["C", "m", "n"]
    , "class (X x) ⇒ C a b where\n\tm ∷ a→b\n\tn ∷ c\n"              ==>
        ["C", "m", "n"]
    , "class (X x) => C a b | a -> b where\n\tm :: a->b\n\tn :: c\n" ==>
        ["C", "m", "n"]
    , "class (X x) ⇒ C a b | a → b where\n\tm ∷ a→b\n\tn ∷ c\n"      ==>
        ["C", "m", "n"]
    , "class A a where f :: X\n"                                     ==>
        ["A", "f"]
      -- indented inside where
    , "class X where\n\ta, (+) :: X\n"                               ==>
        ["+", "X", "a"]
    , "class X where\n\ta :: X\n\tb, c :: Y"                         ==>
        ["X", "a", "b", "c"]
    , "class X\n\twhere\n\ta :: X\n\tb, c :: Y"                      ==>
        ["X", "a", "b", "c"]
    , "class X\n\twhere\n\ta ::\n\t\tX\n\tb :: Y"                    ==>
        ["X", "a", "b"]

    , "class a :<: b where\n    f :: a -> b"                         ==>
        [":<:", "f"]
    , "class (:<:) a b where\n    f :: a -> b"                       ==>
        [":<:", "f"]
    , "class Eq a => a :<: b where\n    f :: a -> b"                 ==>
        [":<:", "f"]
    , "class a ~ 'Foo => a :<: b where\n    f :: a -> b"             ==>
        [":<:", "f"]
    , "class 'Foo ~ a => a :<: b where\n    f :: a -> b"             ==>
        [":<:", "f"]
    , "class (Eq a) => a :<: b where\n    f :: a -> b"               ==>
        [":<:", "f"]
    , "class (a ~ 'Foo) => a :<: b where\n    f :: a -> b"           ==>
        [":<:", "f"]
    , "class ('Foo ~ a) => a :<: b where\n    f :: a -> b"           ==>
        [":<:", "f"]
    , "class a :<<<: b => a :<: b where\n    f :: a -> b"            ==>
        [":<:", "f"]
    , "class (a :<<<: b) => a :<: b where\n    f :: a -> b"   ==> [":<:", "f"]
    , "class (a :<<<: b) ⇒ a :<: b where\n    f ∷ a → b"      ==> [":<:", "f"]
    , "class (Eq a, Ord b) => a :<: b where\n    f :: a -> b" ==> [":<:", "f"]
    , "class (Eq a, Ord b) => (a :: (* -> *) -> *) :<: b where\n    f :: a -> b"
      ==>
      [":<:", "f"]
      -- this is bizzarre
    , "class (Eq (a), Ord (f a [a])) => f `Z` a" ==> ["Z"]

    , "class A f where\n  data F f :: *\n  g :: a -> f a\n  h :: f a -> a"
      ==>
      ["A", "F", "g", "h"]
    , "class A f where\n  data F f :: *\n  mkF :: f -> F f\n  getF :: F f -> f"
      ==>
      ["A", "F", "getF", "mkF"]
    , "class A f where\n\
      \  data F f :: * -- foo\n\
      \                -- bar\n\
      \                -- baz\n\
      \  mkF  :: f -> F f\n\
      \  getF :: F f -> f"
      ==>
      ["A", "F", "getF", "mkF"]
    -- Not confused by a class context on a method.
    , "class X a where\n\tfoo :: Eq a => a -> a\n" ==> ["X", "foo"]
    , "class Category cat where\n\
      \    -- | the identity morphism\n\
      \    id :: cat a a\n\
      \ \n\
      \    -- | morphism composition\n\
      \    (.) :: cat b c -> cat a b -> cat a c" ==> [".", "Category", "id"]
    , "class Match a b where\n\
      \    pattern :: Pattern a b\n" ==> ["Match", "pattern"]
    , "class a ~~ b => (a :: k) ~ (b :: k) | a -> b, b -> a"
      ==>
      ["~"]
    , "class a ~~ b => (a :: k) ! (b :: k) | a -> b, b -> a"
      ==>
      ["!"]
    ]
    where
    (==>) = testTagNames filename

testInstance :: TestTree
testInstance = testGroup "instance"
    [ "instance Foo Quux where\n\
      \  data Bar Quux a = QBar { frob :: a }\n\
      \                  | QBaz { fizz :: String }\n\
      \                  deriving (Show)"
      ==>
      ["QBar", "QBaz", "fizz", "frob"]
    , "instance Foo Quux where\n\
      \  data Bar Quux a = QBar a | QBaz String deriving (Show)"
      ==>
      ["QBar", "QBaz"]
    , "instance Foo Quux where\n\
      \  data Bar Quux a = QBar { frob :: a }\n\
      \                  | QBaz { fizz :: String }\n\
      \                  deriving (Show)\n\
      \  data IMRuunningOutOfNamesHere Quux = Whatever"
      ==>
      ["QBar", "QBaz", "Whatever", "fizz", "frob"]
      -- in this test foo function should not affect tags found
    , "instance Foo Quux where\n\
      \  data Bar Quux a = QBar { frob :: a }\n\
      \                  | QBaz { fizz :: String }\n\
      \                  deriving (Show)\n\
      \\n\
      \  foo _ = QBaz \"hey there\""
      ==>
      ["QBar", "QBaz", "fizz", "frob"]
    , "instance Foo Int where foo _ = 1"
      ==>
      []
    , "instance Foo Quux where\n\
      \  newtype Bar Quux a = QBar a \n\
      \                     deriving (Show)\n\
      \\n\
      \  foo _ = QBaz \"hey there\""
      ==>
      ["QBar"]
    , "instance Foo Quux where\n\
      \  newtype Bar Quux a = QBar { frob :: a }"
      ==>
      ["QBar", "frob"]
    , "instance (Monoid w, MBC b m) => MBC b (JournalT w m) where\n\
      \   newtype StM (JournalT w m) a =\n\
      \       StMJournal { unStMJournal :: ComposeSt (JournalT w) m a }\n\
      \   liftBaseWith = defaultLiftBaseWith StMJournal\n\
      \   restoreM     = defaultRestoreM   unStMJournal\n\
      \   {-# INLINE liftBaseWith #-}\n\
      \   {-# INLINE restoreM #-}\n\
      \"
      ==>
      ["StMJournal", "unStMJournal"]
    ]
    where
    (==>) = testTagNames filename

testLiterate :: TestTree
testLiterate = testGroup "literate"
    [ "> class (X x) => C a b where\n>\tm :: a->b\n>\tn :: c\n"
      ==>
      ["C", "m", "n"]
    , "Test\n\\begin{code}\nclass (X x) => C a b where\n\
      \\tm :: a->b\n\tn :: c\n\\end{code}"
      ==>
      ["C", "m", "n"]
    ]
    where
    (==>) = test f
    f = List.sort . map untag . fst . Tag.process "fn.lhs" False

testPatterns :: TestTree
testPatterns = testGroup "patterns"
    [ "pattern Arrow a b = ConsT \"->\" [a, b]"
      ==>
      ["Arrow"]
    , "pattern Arrow a b = ConsT \"->\" [a, b]\n\
      \pattern Pair a b = [a, b]"
      ==>
      ["Arrow", "Pair"]
    , "pattern Sub a b = Op '-' [a, b]\n\
      \pattern Pair a b = [a, b]"
      ==>
      ["Pair", "Sub"]
    , "pattern (:++) x y = [x, y]"
      ==>
      [":++"]
    , "pattern x :** y = [x, y]"
      ==>
      [":**"]
    , "pattern Nil :: Vec2 a\n\
      \pattern Nil = Vec2 []\n"
      ==>
      ["Nil", "Nil"]
    , "pattern (:>) x xs <- ((\\ys -> (head $ unvec2 ys,Vec2 . tail $ unvec2 ys)) -> (x,xs))\n\
      \where\n\
      \   (:>) x xs = Vec2 (x:unvec2 xs)"
      ==>
      [":>"]
    ]
    where
    (==>) = testTagNames filename

testFFI :: TestTree
testFFI = testGroup "ffi"
    [ "foreign import ccall foo :: Double -> IO Double"            ==> ["foo"]
    , "foreign import unsafe java foo :: Double -> IO Double"      ==> ["foo"]
    , "foreign import safe stdcall foo :: Double -> IO Double"     ==> ["foo"]
    , "foreign import safe stdcall pattern :: Double -> IO Double" ==> ["pattern"]
    ]
    where
    (==>) = testTagNames filename

testStripCpp :: TestTree
testStripCpp = testGroup "strip cpp"
    [ "foo\n\
      \#if 0\n\
      \bar\n\
      \baz\n\
      \#endif\n\
      \quux" ==> "foo\n\nbar\nbaz\n\nquux"
    , "foo\n\
      \#if 0\n\
      \bar\n\
      \#else\n\
      \baz\n\
      \#endif\n\
      \quux" ==> "foo\n\nbar\n\nbaz\n\nquux"
    , "foo\n\
      \#let a t = \"%lu\", (ul)offsetof(struct {char x__; t (y__); }, y__)\n\
      \bar" ==> "foo\n\nbar"
    , "foo\n\
      \#let x = y\\\n\
      \         z\\\n\
      \         w\n\
      \bar" ==> "foo\n\n\n\nbar"
    ]
    where
    (==>) = test stripCpp

test :: (Show a, ExtendedEq b, Show b) => (a -> b) -> a -> b -> TestTree
test f x expected =
    HUnit.testCase (take 70 $ show x) $
    HUnit.assertBool msg (actual === expected)
    where
    actual = f x
    msg    = "expected: " ++ show expected ++ "\n but got: " ++ show actual

filename :: FilePath
filename = "fn.hs"

testFullTagsWithPrefixes :: FilePath -> Text -> [Pos TagVal] -> TestTree
testFullTagsWithPrefixes fn = \source tags ->
    test (first List.sort . Tag.process fn trackPrefixes) source (tags, warnings)
    where
    warnings :: [String]
    warnings = []
    trackPrefixes :: Bool
    trackPrefixes = True

testTagNames :: FilePath -> Text -> [String] -> TestTree
testTagNames fn source tags =
    test process source (tags, warnings)
    where
    warnings :: [String]
    warnings = []

    process :: Text -> ([String], [String])
    process = first (List.sort . map untag) . Tag.process fn trackPrefixes

    trackPrefixes :: Bool
    trackPrefixes = False

untag :: Pos TagVal -> String
untag (Pos _ (TagVal name _ _)) = T.unpack name

tokenize :: Text -> UnstrippedTokens
tokenize =
    either error UnstrippedTokens .
    Lexer.tokenize filename trackPrefixes .
    Tag.stripCpp
    where
    trackPrefixes = False

extractTokens :: UnstrippedTokens -> [Text]
extractTokens = map tokenName . Tag.unstrippedTokensOf
    where
    tokenName token = case Tag.valOf token of
        T name    -> name
        Newline n -> T.pack ("nl " ++ show n)
        t         -> T.pack $ show t
