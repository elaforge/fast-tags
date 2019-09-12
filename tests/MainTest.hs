----------------------------------------------------------------------------
-- |
-- Module      :  MainTest
-- Copyright   :  (c) Sergey Vinokurov 2019
----------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module MainTest where
import Control.Arrow (first)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Void (Void)

import qualified Test.Tasty as Tasty
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.HUnit as HUnit

import qualified FastTags.Emacs as Emacs
import qualified FastTags.Lexer as Lexer
import FastTags.LexerTypes (LitMode(..))
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
    , testWhereBlock
    , testEmacs
    , testVim
    , testProcess
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

instance (ExtendedEq a, ExtendedEq b, ExtendedEq c) => ExtendedEq (a, b, c)
        where
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
    , "x{-\n  bc#-}\n"    ==> [T "x", Newline 0, Newline 0]
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
    -- multiline string with \r
    , "foo \"bar\\\r\n\
      \  \\bar\" baz"     ==> [T "foo", String, T "baz", Newline 0]
    -- multiline string with zero indentation
    , "foo \"bar\\\r\n\
      \\\bar\" baz"     ==> [T "foo", String, T "baz", Newline 0]
    , "(\\err -> case err of Foo -> True; _ -> False)" ==>
        [ LParen, LambdaBackslash, T "err", Arrow, KWCase, T "err", KWOf, T "Foo"
        , Arrow, T "True", Semicolon, T "_", Arrow, T "False", RParen, Newline 0
        ]
    , "foo = \"foo\\n\\\r\n\
      \  \\\" bar" ==> [T "foo", Equals, String, T "bar", Newline 0]
    , "foo = \"foo\\n\\\n\
      \  \\x\" bar" ==>
        [ T "foo", Equals, String, T "bar", Newline 0]

    , "{-   ___   -}import Data.Char;main=putStr$do{c<-\"/1 AA A A;9+ )11929 )1191A 2C9A \";e\r\n\
      \ {- {- | -} -}  {- {- || -} -}{- {- || -} -}{- {- || -} -} {--}.(`divMod`8).(+(-32)).ord$c};f(0,0)=\"\\n\";f(m,n)=m?\"  \"++n?\"_/\"\r\n\
      \{- {- | -} -}n?x=do{[1..n];x}                                    --- obfuscated\r\n\
      \{-\\_/ on Fairbairn, with apologies to Chris Brown. Above is / Haskell 98 -}"
      ==>
      [ KWImport, T "Data.Char", Semicolon
      , T "main", Equals , T "putStr", T "$", KWDo, LBrace, T "c", T "<-"
      , String, Semicolon, T "e", Newline 4
      , Dot, LParen, Backtick, T "divMod", Backtick, Number, RParen, Dot
      , LParen, T "+", LParen, Number, RParen, RParen, Dot
      , T "ord", T "$", T "c", RBrace, Semicolon
      , T "f", LParen, Number, Comma, Number, RParen, Equals, String, Semicolon
      , T "f", LParen, T "m", Comma, T "n", RParen, Equals, T "m", T "?"
      , String, T "++", T "n", T "?", String, Newline 0
      , T "n", T "?", T "x", Equals, KWDo, LBrace, LBracket, Number
      , T "..", T "n", RBracket, Semicolon, T "x", RBrace, Newline 0
      , Newline 0
      ]

    , "one_hash, two_hash :: text_type\n\
      \hash_prec :: Int -> Int\n\
      \one_hash  = from_char '#'\n\
      \two_hash  = from_string \"##\"\n\
      \hash_prec = const 0"
      ==>
      [ T "one_hash", Comma, T "two_hash", DoubleColon, T "text_type", Newline 0
      , T "hash_prec", DoubleColon, T "Int", Arrow, T "Int", Newline 0
      , T "one_hash", Equals, T "from_char", Character, Newline 0
      , T "two_hash",  Equals, T "from_string", String, Newline 0
      , T "hash_prec", Equals, T "const", Number, Newline 0
      ]
    , "showComplexFloat x 0.0 = showFFloat Nothing x \"\""
      ==>
      [ T "showComplexFloat", T "x", Number, Equals
      , T "showFFloat", T "Nothing", T "x", String, Newline 0
      ]
    , "deriveJSON defaultOptions ''CI.CI"
      ==>
      [T "deriveJSON", T "defaultOptions", T "''CI.CI", Newline 0]
    , "--:+: :+: :+:"
      ==>
      [T "--:+:", T ":+:", T ":+:", Newline 0]

    , "\\x -> y" ==> [LambdaBackslash, T "x", Arrow, T "y", Newline 0]
    , "f :: G -> N -> R\n\
      \f g = \n\
      \\\n -> case lookup n info' of\n\
      \\tNothing -> []\n\
      \\tJust c  -> c\n\
      \  where"
      ==>
      [ T "f", DoubleColon, T "G", Arrow, T "N", Arrow, T "R", Newline 0
      , T "f", T "g", Equals, Newline 0
      , LambdaBackslash, T "n", Arrow
      , KWCase, T "lookup", T "n", T "info'", KWOf, Newline 1
      , T "Nothing", Arrow, LBracket, RBracket, Newline 1
      , T "Just", T "c", Arrow, T "c", Newline 2
      , KWWhere, Newline 0
      ]

    , tokenizeSplices
    , "import Foo hiding (Bar)" ==>
      [KWImport, T "Foo", T "hiding", LParen, T "Bar", RParen, Newline 0]

    , "foo\n\
      \#{enum Bar, Baz }\n\
      \quux" ==>
      [ T "foo", Newline 0, HSCEnum, T "Bar", Comma, T "Baz", RBrace, Newline 0
      , T "quux", Newline 0
      ]

    , "newtype ControlOp' = ControlOp' CInt\n\
      \\n\
      \#{enum ControlOp, ControlOp\n\
      \ , controlOpAdd    = EPOLL_CTL_ADD\n\
      \ , controlOpModify = EPOLL_CTL_MOD\n\
      \ , controlOpDelete = EPOLL_CTL_DEL\n\
      \ }" ==>
      [ KWNewtype, T "ControlOp'", Equals, T "ControlOp'", T "CInt", Newline 0
      , Newline 0
      , HSCEnum, T "ControlOp", Comma, T "ControlOp", Newline 1
      , Comma, T "controlOpAdd", Equals, T "EPOLL_CTL_ADD", Newline 1
      , Comma, T "controlOpModify", Equals, T "EPOLL_CTL_MOD", Newline 1
      , Comma, T "controlOpDelete", Equals, T "EPOLL_CTL_DEL", Newline 1
      , RBrace, Newline 0
      ]
    , "a # b = apply a b" ==>
      [T "a", T "#", T "b", Equals, T "apply", T "a", T "b", Newline 0]

    , "\"\""         ==> [String, Newline 0]
    , "\"a\""        ==> [String, Newline 0]
    , "\"abc\""      ==> [String, Newline 0]
    , "\"abc\\n\""   ==> [String, Newline 0]
    , "\"abc\\d\""   ==> [String, Newline 0]
    , "\"abc\\\\d\"" ==> [String, Newline 0]
    , "\"\\\\d\""    ==> [String, Newline 0]
    , "\"\\\\\""     ==> [String, Newline 0]

    , "\"a\" ++ \"b\""  ==> [String, T "++", String, Newline 0]
    , "\"\\\\\" ++ \"b\"" ==> [String, T "++", String, Newline 0]

    , "'a'"       ==> [Character, Newline 0]
    , "'\\''"     ==> [Character, Newline 0]
    , "'\"'"      ==> [Character, Newline 0]
    , "'\\\\'"    ==> [Character, Newline 0]
    , "foo' bar'" ==> [T "foo'", T "bar'", Newline 0]
    , "'\\n'"     ==> [Character, Newline 0]

    , "argsOf s = map (init . (drop $ 2 + length s)) . filter ((\"\\\\\" ++ s ++ \"{\") `isPrefixOf`)"
      ==>
      [ T "argsOf", T "s", Equals, T "map", LParen, T "init", Dot
      , LParen, T "drop", T "$", Number, T "+"
      , T "length", T "s", RParen, RParen , Dot, T "filter"
      , LParen, LParen, String, T "++", T "s", T "++", String, RParen
      , Backtick, T "isPrefixOf", Backtick, RParen, Newline 0
      ]

    , "foo\n\
      \# 17 \"/usr/include/stdc-predef.h\" 3 4\n\
      \bar"
      ==>
      [ T "foo", Newline 0
      , Newline 0
      , T "bar", Newline 0
      ]
    , "foo\n\
      \#{get_area \"Queue.T\"}\n\
      \bar"
      ==>
      [ T "foo", Newline 0
      , Newline 0
      , T "bar", Newline 0
      ]
    , "foo\n\
      \#ccall apr_atomic_init, Ptr <apr_pool_t> -> IO <apr_status_t>\n\
      \bar"
      ==>
      [ T "foo", Newline 0
      , Newline 0
      , T "bar", Newline 0
      ]
    ]
    where
    (==>) = test f
    f :: Text -> [TokenVal]
    f = tail -- strip uninteresting initial newline
      . map valOf
      . unstrippedTokensOf
      . tokenize

    tokenizeSplices = testGroup "tokenize splices"
        [ "$(foo)"                                  ==>
            [SpliceStart, T "foo", RParen, Newline 0]
        , "$(foo [| baz |])"                        ==>
            [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
            , Newline 0
            ]
        , "$(foo ⟦ baz ⟧)"                          ==>
            [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
            , Newline 0
            ]
        , "$(foo [bar| baz |])"                     ==>
            [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
            , Newline 0
            ]
        , "$(foo [Foo.bar| baz ⟧)"                  ==>
            [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
            , Newline 0
            ]
        , "$(foo [$bar| baz |])"                    ==>
            [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
            , Newline 0
            ]
        , "$(foo [$Foo.bar| baz ⟧)"                 ==>
            [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
            , Newline 0
            ]
        , "$(foo [bar| bar!\nbaz!\n $(baz) |])"     ==>
             [ SpliceStart, T "foo", QuasiquoterStart, SpliceStart, T "baz"
             , RParen, QuasiquoterEnd, RParen, Newline 0
             ]
        , "$(foo [Foo.bar| bar!\nbaz!\n $(baz) ⟧)"  ==>
             [ SpliceStart, T "foo", QuasiquoterStart, SpliceStart, T "baz"
             , RParen, QuasiquoterEnd, RParen, Newline 0
             ]
        , "$(foo [$bar| bar!\nbaz!\n $(baz) |])"    ==>
             [ SpliceStart, T "foo", QuasiquoterStart, SpliceStart, T "baz"
             , RParen, QuasiquoterEnd, RParen, Newline 0
             ]
        , "$(foo [$Foo.bar| bar!\nbaz!\n $(baz) ⟧)" ==>
             [ SpliceStart, T "foo", QuasiquoterStart, SpliceStart, T "baz"
             , RParen, QuasiquoterEnd, RParen, Newline 0
             ]
        , "foo [$bar| baz |]"                       ==>
             [ T "foo", QuasiquoterStart, QuasiquoterEnd, Newline 0 ]
        , "foo [$bar|\n\
          \ baz |]"                                 ==>
             [ T "foo", QuasiquoterStart, QuasiquoterEnd, Newline 0 ]
        , "foo [$bar|\n\
          \ baz \n\
          \|]"                                      ==>
             [ T "foo", QuasiquoterStart, QuasiquoterEnd, Newline 0 ]
        , "foo\n\
          \$bar\n\
          \baz"                                     ==>
             [ T "foo", Newline 0
             , ToplevelSplice, Newline 0
             , T "baz", Newline 0
             ]
        , "foo\nf $ x = f x\nbaz"                   ==>
             [ T "foo", Newline 0
             , T "f", T "$", T "x", Equals, T "f", T "x", Newline 0
             , T "baz", Newline 0
             ]
        ]

testTokenizeWithNewlines :: TestTree
testTokenizeWithNewlines = testGroup "tokenize with newlines"
    [ testGroup "vanilla"
        [ "x\ny\n"
          ==> [Newline 0, T "x", Newline 0, T "y", Newline 0, Newline 0]
        , " xx\n yy\n"
          ==> [Newline 1, T "xx", Newline 1, T "yy", Newline 0, Newline 0]
        , "\n\
          \class (X x) => C a b where\n\
          \\tm :: a->b\n\
          \\tn :: c\n"
          ==>
          [ Newline 0
          , Newline 0
          , KWClass, LParen, T "X", T "x", RParen, Implies
          , T "C", T "a", T "b", KWWhere, Newline 1
          , T "m", DoubleColon, T "a", Arrow, T "b", Newline 1
          , T "n", DoubleColon, T "c"
          , Newline 0
          , Newline 0
          ]
        ]
    , testGroup "literate"
        [ "foo bar baz"
          |=>
          []
        , "foo bar baz\nquux fizz buzz"
          |=>
          []
        , "> foo = 1"
          |=>
          [Newline 1, T "foo", Equals, Number, Newline 0]
        , "This is a factorial function\n\
          \\n\
          \> f :: Integer -> Integer\n\
          \> f 0 = 1\n\
          \> f n = \n\
          \>   n * (f $ n - 1)\n\
          \\n\
          \And that's it !"
          |=>
          [ Newline 1
          , T "f", DoubleColon, T "Integer", Arrow, T "Integer", Newline 1
          , T "f", Number, Equals, Number, Newline 1
          , T "f", T "n", Equals, Newline 3
          , T "n", T "*", LParen, T "f", T "$"
          , T "n", T "-", Number, RParen, Newline 0
          ]
        , "This is a factorial function\n\
          \\n\
          \> f :: Integer -> Integer\n\
          \> f 0 = 1\n\
          \> f n = \n\
          \>   n * (f $ n - 1)\n\
          \\n\
          \And another function:\n\
          \\n\
          \> foo :: a -> a\n\
          \> foo x = x"
          |=>
          [ Newline 1
          , T "f", DoubleColon, T "Integer", Arrow, T "Integer", Newline 1
          , T "f", Number, Equals, Number, Newline 1
          , T "f", T "n", Equals, Newline 3
          , T "n", T "*", LParen, T "f", T "$"
          , T "n", T "-", Number, RParen, Newline 0
          , Newline 1
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 1
          , T "foo", T "x", Equals, T "x", Newline 0
          ]

        , "    This is a factorial function\n\
          \\n\
          \> f :: Integer -> Integer\n\
          \> f 0 = 1\n\
          \> f n = \n\
          \>   n * (f $ n - 1)\n\
          \\n\
          \    And another function:\n\
          \\n\
          \> foo :: a -> a\n\
          \> foo x = x"
          |=>
          [ Newline 1
          , T "f", DoubleColon, T "Integer", Arrow, T "Integer", Newline 1
          , T "f", Number, Equals, Number, Newline 1
          , T "f", T "n", Equals, Newline 3
          , T "n", T "*", LParen, T "f", T "$"
          , T "n", T "-", Number, RParen, Newline 0
          , Newline 1
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 1
          , T "foo", T "x", Equals, T "x", Newline 0
          ]

        , "This is a factorial function\n\
          \\\begin{code}\n\
          \f :: Integer -> Integer\n\
          \f 0 = 1\n\
          \f n = \n\
          \  n * (f $ n - 1)\n\
          \\\end{code}\n\
          \And that's it !"
          |=>
          [ Newline 0
          , T "f", DoubleColon, T "Integer", Arrow, T "Integer", Newline 0
          , T "f", Number, Equals, Number, Newline 0
          , T "f", T "n", Equals, Newline 2
          , T "n", T "*", LParen, T "f", T "$", T "n", T "-", Number, RParen
          ]
        , "This is a 'factorial' function\n\
          \\\begin{code}\n\
          \f :: Integer -> Integer\n\
          \f 0 = 1\n\
          \f n = \n\
          \  n * (f $ n - 1)\n\
          \\\end{code}\n\
          \But that's not it yet! Here's another function:\n\
          \\\begin{code}\n\
          \foo :: a -> a\n\
          \foo x = x\n\
          \\\end{code}\n\
          \And that's it !"
          |=>
          [ Newline 0
          , T "f", DoubleColon, T "Integer", Arrow, T "Integer", Newline 0
          , T "f", Number, Equals, Number, Newline 0
          , T "f", T "n", Equals, Newline 2
          , T "n", T "*", LParen, T "f", T "$"
          , T "n", T "-", Number, RParen, Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x"
          ]
        , "This is a 'factorial' function\n\
          \\\begin{code}\n\
          \  f :: Integer -> Integer\n\
          \  f 0 = 1\n\
          \  f n = \n\
          \    n * (f $ n - 1)\n\
          \\\end{code}\n\
          \But that's not it yet! Here's another function:\n\
          \\\begin{code}\n\
          \  foo :: a -> a\n\
          \  foo x = x\n\
          \\\end{code}\n\
          \And that's it !"
          |=>
          [ Newline 2
          , T "f", DoubleColon, T "Integer", Arrow, T "Integer", Newline 2
          , T "f", Number, Equals, Number, Newline 2
          , T "f", T "n", Equals, Newline 4
          , T "n", T "*", LParen, T "f", T "$", T "n", T "-", Number, RParen

          , Newline 2
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 2
          , T "foo", T "x", Equals, T "x"
          ]
        , "Test\n\
          \\\begin{code}\n\
          \class (X x) => C a b where\n\
          \  m :: a->b\n\
          \  n :: c\n\
          \\\end{code}"
          |=>
          [ Newline 0
          , KWClass, LParen, T "X", T "x", RParen, Implies
          , T "C", T "a", T "b", KWWhere, Newline 2
          , T "m", DoubleColon, T "a", Arrow, T "b", Newline 2
          , T "n", DoubleColon, T "c"
          ]
        , "Test\n\
          \\\begin{code}\n\
          \class (X x) => C a b where\n\
          \\tm :: a->b\n\
          \\tn :: c\n\
          \\\end{code}"
          |=>
          [ Newline 0
          , KWClass, LParen, T "X", T "x", RParen, Implies
          , T "C", T "a", T "b", KWWhere, Newline 1
          , T "m", DoubleColon, T "a", Arrow, T "b", Newline 1
          , T "n", DoubleColon, T "c"
          ]
        ]
    ]
    where
    (==>) = test (f LitVanilla)
    (|=>) = test (f LitOutside)
    f mode = map valOf . unstrippedTokensOf . tokenizeWith mode


testStripComments :: TestTree
testStripComments = testGroup "Comments stripping"
    [ "hello -- there"
      ==>
      [Newline 0, T "hello", Newline 0]
    , "hello --there"
      ==>
      [Newline 0, T "hello", Newline 0]
    , "hello {- there -} fred"
      ==>
      [Newline 0, T "hello", T "fred", Newline 0]
    , "hello -- {- there -}\n\
      \fred"
      ==>
      [Newline 0, T "hello", Newline 0, T "fred", Newline 0]
    , "{-# LANG #-} hello {- there {- nested -} comment -} fred"
      ==>
      [Newline 1, T "hello", T "fred", Newline 0]
    , "hello {-\n\
      \there\n\
      \------}\n\
      \ fred"
      ==>
      [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
    , "hello {-  \n\
      \there\n\
      \  ------}  \n\
      \ fred"
      ==>
      [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
    , "hello {-\n\
      \there\n\
      \-----}\n\
      \ fred"
      ==>
      [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
    , "hello {-  \n\
      \there\n\
      \  -----}  \n\
      \ fred"
      ==>
      [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
    , "hello {-\n\
      \-- there -}"
      ==>
      [Newline 0, T "hello", Newline 0]
    , "foo --- my comment\n\
      \--- my other comment\n\
      \bar"
      ==>
      [Newline 0, T "foo", Newline 0, Newline 0, T "bar", Newline 0]
    ]
    where
    (==>) = test f
    f = map valOf . unstrippedTokensOf . tokenize

testBreakBlocks :: TestTree
testBreakBlocks = testGroup "Break blocks"
  [ testGroup "vanilla"
    [ "a\n\
      \b\n"
      ==>
      [ [T "a"]
      , [T "b"]
      ]
    , "a\n\
      \ a\n\
      \b\n"
      ==>
      [ [T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    , "a\n\
      \ a\n\
      \ a\n\
      \b\n"
      ==>
      [ [T "a", Newline 1, T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    -- intervening blank lines are ignored
    , "a\n\
      \ a\n\
      \\n\
      \ a\n\
      \b\n"
      ==>
      [ [T "a", Newline 1, T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    , "a\n\
      \\n\
      \\n\
      \ a\n\
      \b\n"
      ==>
      [ [T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    , "a\n\
      \ aa\n\
      \ aa\n"
      ==>
      [[T "a", Newline 1, T "aa", Newline 1, T "aa"]]
    , " aa\n\
      \ aa\n"
      ==>
      [ [T "aa"]
      , [T "aa"]
      ]

    , "one_hash, two_hash :: text_type\n\
      \hash_prec :: Int -> Int\n\
      \one_hash  = from_char '#'\n\
      \two_hash  = from_string \"##\"\n\
      \hash_prec = const 0"
      ==>
      [ [T "one_hash", Comma, T "two_hash", DoubleColon, T "text_type"]
      , [T "hash_prec", DoubleColon, T "Int", Arrow, T "Int"]
      , [T "one_hash", Equals, T "from_char", Character]
      , [T "two_hash",  Equals, T "from_string", String]
      , [T "hash_prec", Equals, T "const", Number]
      ]
    , "one_hash, two_hash :: text_type; \
      \hash_prec :: Int -> Int; \
      \one_hash  = from_char '#'; \
      \two_hash  = from_string \"##\"; \
      \hash_prec = const 0"
      ==>
      [ [T "one_hash", Comma, T "two_hash", DoubleColon, T "text_type"]
      , [T "hash_prec", DoubleColon, T "Int", Arrow, T "Int"]
      , [T "one_hash", Equals, T "from_char", Character]
      , [T "two_hash",  Equals, T "from_string", String]
      , [T "hash_prec", Equals, T "const", Number]
      ]
    , "{\n\
      \  data F f :: * ; -- foo\n\
      \                  -- bar\n\
      \                  -- baz\n\
      \  mkF  :: f -> F f ; getF :: F f -> f ;\n\
      \} ;"
      ==>
      [ [ LBrace, Newline 2, KWData, T "F", T "f", DoubleColon
        , T "*", Semicolon, Newline 2
        , T "mkF", DoubleColon, T "f", Arrow, T "F", T "f", Semicolon
        , T "getF", DoubleColon, T "F", T "f", Arrow, T "f", Semicolon
        , Newline 0
        , RBrace
        ]
        ]
      ]
  , testGroup "literate"
    [ "> a\n\
      \>\n\
      \>\n\
      \>  a\n\
      \> b\n"
      |=>
      [ [T "a", Newline 2, T "a"]
      , [T "b"]
      ]
    , "> a\n\
      \> \n\
      \> \n\
      \>  a\n\
      \> b\n"
      |=>
      [ [T "a", Newline 2, T "a"]
      , [T "b"]
      ]
    , "> a\n\
      \>  aa\n\
      \>  aa\n"
      |=>
      [[T "a", Newline 2, T "aa", Newline 2, T "aa"]]
    , "> a\n\
      \>  aa\n\
      \>\n\
      \>  aa\n"
      |=>
      [[T "a", Newline 2, T "aa", Newline 2, T "aa"]]
   ]
    ]
    where
    (==>) = test (f LitVanilla)
    (|=>) = test (f LitOutside)
    f :: LitMode Void -> Text -> [[TokenVal]]
    f mode
        = map (map valOf . unstrippedTokensOf)
        . breakBlocks ProcessVanilla
        . tokenizeWith mode

testWhereBlock :: TestTree
testWhereBlock = testGroup "whereBlock"
    [ "class A f where\n\
      \  data F f :: * -- foo\n\
      \                -- bar\n\
      \                -- baz\n\
      \  mkF  :: f -> F f\n\
      \  getF :: F f -> f"
      ==>
      [ [KWData, T "F", T "f", DoubleColon, T "*"]
      , [T "mkF", DoubleColon, T "f", Arrow, T "F", T "f"]
      , [T "getF", DoubleColon, T "F", T "f", Arrow, T "f"]
      ]
    , "class A f where {\n\
      \  data F f :: * ; -- foo\n\
      \                  -- bar\n\
      \                  -- baz\n\
      \  mkF  :: f -> F f ;\n\
      \  getF :: F f -> f ;\n\
      \} ;"
      ==>
      [ [KWData, T "F", T "f", DoubleColon, T "*"]
      , [T "mkF", DoubleColon, T "f", Arrow, T "F", T "f"]
      , [T "getF", DoubleColon, T "F", T "f", Arrow, T "f"]
      ]
    ]
    where
    (==>) = test f
    f = map (map valOf . Tag.stripNewlines)
      . Tag.whereBlock
      . tokenize

testEmacs :: TestTree
testEmacs = testGroup "Emacs" [emacsFormat]

emacsFormat :: TestTree
emacsFormat = testGroup "format"
  [
    test Emacs.formatTag (Pos tag val) $ T.concat ["wibble wobble", "\x7f", "wibble wobble", "\x01", "10,14"]
  ]
  where tag = (SrcPos "fn" 10 20 "wibble" " wobble")
        val = (TagVal "wibble wobble" Tag.Function Nothing)

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
        Vim.parseTag $ Vim.showTag $
        Pos (SrcPos fn (Line line) (Offset 0) "" "") (TagVal text typ Nothing)

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
    makeTag (fn, line, text) =
      Pos (SrcPos fn (Line line) (Offset 0) "" "") (TagVal text Function Nothing)

testProcess :: TestTree
testProcess = testGroup "process"
    [ testMeta
    , testData
    , testGADT
    , testFamilies
    , testFunctions
    , testClass
    , testInstance
    , testLiterate
    , testPatterns
    , testFFI
    , testDefine
    , testHSC2HS
    , testAlex
    , testHappy
    ]

testMeta :: TestTree
testMeta = testGroup "prefix, suffix and offset tracking"
    [ "module Bar.Foo where\n" ==>
        [Pos (SrcPos fn 1 14 "module Bar.Foo" " where") (TagVal "Foo" Module Nothing)]
    , "newtype Foo a b =\n\
      \\tBar x y z\n" ==>
        [ Pos (SrcPos fn 1 11 "newtype Foo" " a b =") (TagVal "Foo" Type Nothing)
        , Pos (SrcPos fn 2 22 "\tBar" " x y z") (TagVal "Bar" Constructor (Just "Foo"))
        ]
    , "data Foo a b =\n\
      \\tBar x y z\n" ==>
        [ Pos (SrcPos fn 1 8 "data Foo" " a b =") (TagVal "Foo" Type Nothing)
        , Pos (SrcPos fn 2 19 "\tBar" " x y z") (TagVal "Bar" Constructor (Just "Foo"))
        ]
    , "f :: A -> B\n\
      \g :: C -> D\n\
      \data D = C {\n\
      \\tf :: A\n\
      \\t}\n" ==>
        [ Pos (SrcPos fn 1 1 "f" " :: A -> B") (TagVal "f" Function Nothing)
        , Pos (SrcPos fn 2 13 "g" " :: C -> D") (TagVal "g" Function Nothing)
        , Pos (SrcPos fn 3 30 "data D" " = C {") (TagVal "D" Type Nothing)
        , Pos (SrcPos fn 3 34 "data D = C" " {") (TagVal "C" Constructor (Just "D"))
        , Pos (SrcPos fn 4 39 "\tf" " :: A") (TagVal "f" Function (Just "D"))
        ]
    , "instance Foo Bar where\n\
      \  newtype FooFam Bar = BarList [Int]" ==>
        [ Pos (SrcPos fn 2 53 "  newtype FooFam Bar = BarList" " [Int]")
              (TagVal "BarList" Constructor (Just "FooFam"))
        ]
    , "instance Foo Bar where\n\
      \  newtype FooFam Bar = BarList { getBarList :: [Int] }" ==>
        [ Pos (SrcPos fn 2 53 "  newtype FooFam Bar = BarList" " { getBarList :: [Int] }")
              (TagVal "BarList" Constructor (Just "FooFam"))
        , Pos (SrcPos fn 2 66 "  newtype FooFam Bar = BarList { getBarList" " :: [Int] }")
              (TagVal "getBarList" Function (Just "FooFam"))
        ]
    , "instance Foo Bar where\n\
      \  data (Ord a) => FooFam Bar a = BarList { getBarList :: [a] }\n\
      \                               | BarMap { getBarMap :: Map a Int }" ==>
        [ Pos (SrcPos fn 2 63 "  data (Ord a) => FooFam Bar a = BarList" " { getBarList :: [a] }")
              (TagVal "BarList" Constructor (Just "FooFam"))
        , Pos (SrcPos fn 2 76 "  data (Ord a) => FooFam Bar a = BarList { getBarList" " :: [a] }")
              (TagVal "getBarList" Function (Just "FooFam"))
        , Pos (SrcPos fn 3 125 "                               | BarMap" " { getBarMap :: Map a Int }")
              (TagVal "BarMap" Constructor (Just "FooFam"))
        , Pos (SrcPos fn 3 137 "                               | BarMap { getBarMap" " :: Map a Int }")
              (TagVal "getBarMap" Function (Just "FooFam"))
        ]
    , "newtype instance FooFam Bar = BarList { getBarList :: [Int] }" ==>
        [ Pos (SrcPos fn 1 37 "newtype instance FooFam Bar = BarList" " { getBarList :: [Int] }")
              (TagVal "BarList" Constructor (Just "FooFam"))
        , Pos (SrcPos fn 1 50 "newtype instance FooFam Bar = BarList { getBarList" " :: [Int] }")
              (TagVal "getBarList" Function (Just "FooFam"))
        ]
    , "data instance (Ord a) => FooFam Bar a = BarList { getBarList :: [a] }\n\
      \                                      | BarMap { getBarMap :: Map a Int }"
      ==>
      [ Pos (SrcPos fn 1 47 "data instance (Ord a) => FooFam Bar a = BarList" " { getBarList :: [a] }")
            (TagVal "BarList" Constructor (Just "FooFam"))
      , Pos (SrcPos fn 1 60 "data instance (Ord a) => FooFam Bar a = BarList { getBarList" " :: [a] }")
            (TagVal "getBarList" Function (Just "FooFam"))
      , Pos (SrcPos fn 2 116 "                                      | BarMap" " { getBarMap :: Map a Int }")
            (TagVal "BarMap" Constructor (Just "FooFam"))
      , Pos (SrcPos fn 2 128 "                                      | BarMap { getBarMap" " :: Map a Int }")
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
    , "data R = R { a, b::X }"              ==> ["R", "R", "a", "b"]
    , "data R = R { a∷X, b∷Y }"             ==> ["R", "R", "a", "b"]
    , "data R = R { a, b∷X }"               ==> ["R", "R", "a", "b"]
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
    , "newtype () => X a = Add a"                                       ==>
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

    , "newtype () => ((u :: (* -> *) -> *) :*: v) z = X"                ==>
        [":*:", "X"]
    , "data () => ((u :: (* -> *) -> *) :*: v) z = X"                   ==>
        [":*:", "X"]
    , "type () => ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"           ==>
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
    , "data IO a = IO (World->(a,World))"
      ==>
      ["IO", "IO"]

    , "data SubTransformTriple a =\n\
      \        SubTransformTriple\n\
      \           (forall sh. (Shape sh, Slice sh) => Transform (sh:.Int) a)\n\
      \           (forall sh. (Shape sh, Slice sh) => Transform (sh:.Int) a)\n\
      \           (forall sh. (Shape sh, Slice sh) => Transform (sh:.Int) a)"
      ==>
      ["SubTransformTriple", "SubTransformTriple"]

    , "data TestCase \n\
      \    = forall a prop . (Testable prop, Data a) \n\
      \    => TestCase  (((String, a, a) -> Property) -> prop)\n\
      \        deriving (Typeable)"
      ==>
      ["TestCase", "TestCase"]

    , "-- | Binding List\n\
      \data BindingList v a = Variable v => BindingList {source :: Source v a -- ^ the list's binding source\n\
      \                                                , list   :: v [v a]    -- ^ the bound list\n\
      \                                                , pos    :: v Int}     -- ^ the current position"
      ==>
      ["BindingList", "BindingList", "list", "pos", "source"]

    , "data Tester a = Tester\n\
      \    {(===) :: [String] -> a -> IO ()\n\
      \    ,fails :: [String] -> IO ()\n\
      \    ,isHelp :: [String] -> [String] -> IO ()\n\
      \    ,isHelpNot :: [String] -> [String] -> IO ()\n\
      \    ,isVersion :: [String] -> String -> IO ()\n\
      \    ,isVerbosity :: [String] -> Verbosity -> IO ()\n\
      \    ,completion :: [String] -> (Int,Int) -> [Complete] -> IO ()\n\
      \    }"
      ==>
      ["===", "Tester", "Tester", "completion", "fails", "isHelp", "isHelpNot", "isVerbosity", "isVersion"]

    , "data Tester a = Tester\n\
      \    {(===) :: [String] -> a -> IO ()\n\
      \    ,fails :: [String] -> IO ()\n\
      \    ,isHelp, isHelpNot :: [String] -> [String] -> IO ()\n\
      \    ,isVersion :: [String] -> String -> IO ()\n\
      \    ,isVerbosity :: [String] -> Verbosity -> IO ()\n\
      \    ,completion :: [String] -> (Int,Int) -> [Complete] -> IO ()\n\
      \    }"
      ==>
      ["===", "Tester", "Tester", "completion", "fails", "isHelp", "isHelpNot", "isVerbosity", "isVersion"]

    , "-- | View of the right end of a sequence.\n\
      \data ViewR s a\n\
      \    = EmptyR\n\
      \    | s a :> a"
      ==>
      [":>", "EmptyR", "ViewR"]
    , "-- | View of the right end of a sequence.\n\
      \data ViewR s a\n\
      \    = s a :> a\n\
      \    | EmptyR"
      ==>
      [":>", "EmptyR", "ViewR"]

    , "data [] a = [] | a : [a]"
      ==> [":", "[]", "[]"]
    , "data () = ()"
      ==> ["()", "()"]
    , "data (,) a b = (,) a b"
      ==> ["(,)", "(,)"]
    , "data (,,) a b c = (,,) a b c"
      ==> ["(,,)", "(,,)"]
    , "data (a, b) = (a, b)"
      ==> ["(,)", "(,)"]
    , "data (a, b, c) = (a, b, c)"
      ==> ["(,,)", "(,,)"]
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
      \  (:+.) :: Int -> Int -> Vec Int -> Vec Int\n"
      ==>
      [":+.", ":::", "Nil", "Vec"]
    , "data Vec ix where\n\
      \  Nil   :: Int -> Foo Int\n\
      \  -- foo\n\
      \  (:::) :: Int -> Vec Int -> Vec Int\n\
      \-- bar\n\
      \  (:+.) :: Int     -> \n\
      \           -- ^ baz\n\
      \           Int     -> \n\
      \           Vec Int -> \n\
      \Vec Int\n"
      ==>
      [":+.", ":::", "Nil", "Vec"]
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
    , "newtype TyConProxy a b where\n\
      \    TyConProxy :: () -> TyConProxy a b\n\
      \  deriving ( Arbitrary\n\
      \           , Show\n\
      \           , Generic\n\
      \#if defined(__LANGUAGE_DERIVE_GENERIC1__)\n\
      \           , Generic1\n\
      \#endif\n\
      \           )"
      ==> ["TyConProxy", "TyConProxy"]
    , "data Foo a where\n\
      \  Bar :: Baz a => { foo :: Int, bar :: a } -> Foo a"
      ==> ["Bar", "Foo", "bar", "foo"]
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
    , "(+::) :: X -> Y" ==> ["+::"]
    , "(->:) :: X -> Y" ==> ["->:"]
    , "(--+) :: X -> Y" ==> ["--+"]
    , "(=>>) :: X -> Y" ==> ["=>>"]

    -- Multi-line with semicolons at the end
    , "one_hash, two_hash :: text_type;\n\
      \hash_prec :: Int -> Int;\n\
      \one_hash  = from_char '#';\n\
      \two_hash  = from_string \"##\";\n\
      \hash_prec = const 0"
      ==>
      ["hash_prec", "one_hash", "two_hash"]
    -- Single-line separated by semicolons - e.g. result of a macro expansion
    , "one_hash, two_hash :: text_type; \
      \hash_prec :: Int -> Int; \
      \one_hash  = from_char '#'; \
      \two_hash  = from_string \"##\"; \
      \hash_prec = const 0"
      ==>
      ["hash_prec", "one_hash", "two_hash"]

    , "assertDataFormatError :: DecompressError -> IO String\n\
      \assertDataFormatError (DataFormatError detail) = return detail\n\
      \assertDataFormatError _                        = assertFailure \"expected DataError\"\n\
      \                                              >> return \"\"\n"
      ==>
      ["assertDataFormatError"]

    , "instance PartialComparison Double where\n\
      \    type PartialCompareEffortIndicator Double = ()\n\
      \    pCompareEff _ a b = Just $ toPartialOrdering $ Prelude.compare a b\n\
      \--        case (isNaN a, isNaN b) of\n\
      \--           (False, False) -> Just $ toPartialOrdering $ Prelude.compare a b  \n\
      \--           (True, True) -> Just EQ\n\
      \--           _ -> Just NC \n\
      \    pCompareDefaultEffort _ = ()\n\
      \\n\
      \pComparePreludeCompare _ a b =\n\
      \    Just $ toPartialOrdering $ Prelude.compare a b\n\
      \\n\
      \propPartialComparisonReflexiveEQ :: \n\
      \    (PartialComparison t) =>\n\
      \    t -> \n\
      \    (PartialCompareEffortIndicator t) -> \n\
      \    (UniformlyOrderedSingleton t) -> \n\
      \    Bool\n\
      \propPartialComparisonReflexiveEQ _ effort (UniformlyOrderedSingleton e) = \n\
      \    case pCompareEff effort e e of Just EQ -> True; Nothing -> True; _ -> False"
      ==>
      ["pComparePreludeCompare", "propPartialComparisonReflexiveEQ"]

    , "hexQuad :: Z.Parser Int\n\
      \hexQuad = do\n\
      \  s <- Z.take 4\n\
      \  let hex n | w >= C_0 && w <= C_9 = w - C_0\n\
      \            | w >= C_a && w <= C_f = w - 87\n\
      \            | w >= C_A && w <= C_F = w - 55\n\
      \            | otherwise          = 255\n\
      \        where w = fromIntegral $ B.unsafeIndex s n\n\
      \      a = hex 0; b = hex 1; c = hex 2; d = hex 3\n\
      \  if (a .|. b .|. c .|. d) /= 255\n\
      \    then return $! d .|. (c `shiftL` 4) .|. (b `shiftL` 8) .|. (a `shiftL` 12)\n\
      \    else fail \"invalid hex escape\"\n"
      ==>
      ["hexQuad"]

    -- Semicolon within instance does not produce toplevel tags
    , "instance Path  T.Text      where readPath = Just; pathRep _ = typeRep (Proxy :: Proxy Text)"
      ==>
      []

    , "prop_bounds2 o1 w1 o2 w2 = let n1 = fromW8 w1 ; n2 = fromW8 w2 ; bs = ((o1, o2), (o1 + n1, o2 + n2)) in bs == bounds (listArray bs (take ((n1 + 1) * (n2 + 1)) (cycle [False, True, True])))"
      ==>
      ["prop_bounds2"]

    , "string :: String -> ReadP r String\n\
      \-- ^ Parses and returns the specified string.\n\
      \string this = do s <- look; scan this s\n\
      \ where\n\
      \  scan []     _               = do return this\n\
      \  scan (x:xs) (y:ys) | x == y = do get >> scan xs ys\n\
      \  scan _      _               = do pfail"
      ==>
      ["string"]

    , "-- | Get every node of a tree, put it into a list.\n\
      \climb :: Tree a -> [a]\n\
      \climb x = case x of (Empty) -> [];(Branch a Empty b) -> a : climb b;\n\
      \                    (Branch a b Empty) -> a : climb b;\n\
      \                    (Branch a b d) -> a : climb b ++ climb d"
      ==>
      ["climb"]

    , "addend hid a (NotM (App uid okh elr as)) = NotM $ App uid okh elr (f as)\n\
      \ where f (NotM ALNil) = NotM $ ALCons hid a (NotM $ ALNil)\n\
      \       f (NotM (ALCons hid a as)) = NotM $ ALCons hid a (f as)\n\
      \       f _ = __IMPOSSIBLE__\n\
      \addend _ _ _ = __IMPOSSIBLE__\n\
      \copyarg _ = False"
      ==>
      ["addend", "copyarg"]

    , "realWorldTc :: TyCon; \\\n\
      \realWorldTc = mkTyCon3 \"ghc-prim\" \"GHC.Types\" \"RealWorld\"; \\\n\
      \instance Typeable RealWorld where { typeOf _ = mkTyConApp realWorldTc [] }"
      ==>
      ["realWorldTc"]

    , "(r `mkQ` br) a = _" ==> ["mkQ"]

    , "_?_ = return unsafePerformIO" ==> ["?"]

    , "(+) :: DebMap -> String -> DebianVersion\n\
      \m + k = maybe (error (\"No version number for \" ++ show k ++ \" in \" ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)"
      ==>
      ["+"]

    , "(!) :: DebMap -> String -> DebianVersion\n\
      \_ ! k = maybe (error (\"No version number for \" ++ show k ++ \" in \" ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)"
      ==>
      ["!"]

    , "(.) :: DebMap -> String -> DebianVersion\n\
      \m . k = maybe (error (\"No version number for \" ++ show k ++ \" in \" ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)"
      ==>
      ["."]

    , "(.) :: DebMap -> String -> DebianVersion\n\
      \m . k x = maybe (error (\"No version number for \" ++ show k ++ \" in \" ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)"
      ==>
      ["."]

    , "{- 123___ -}import Data.Char;main=putStr$do{c<-\"/1 AA A A;9+ )11929 )1191A 2C9A \";e\n\
      \{-  |  -}    .(`divMod`8).(+(-32)).ord$c};f(0,0)=\"\\n\";f(m,n)=m?\"  \"++n?\"_/\"\n\
      \{-  |  -}n?x=do{[1..n];x}                                    --- obfuscated\n\
      \{-\\_/ on Fairbairn, with apologies to Chris Brown. Above is / Haskell 98 -}"
      ==>
      ["?", "f", "main"]
    , "{-   456___   -}import Data.Char;main=putStr$do{c<-\"/1 AA A A;9+ )11929 )1191A 2C9A \";e\n\
      \ {- {- | -} -}  {- {- || -} -}{- {- || -} -}{- {- || -} -} {--}.(`divMod`8).(+(-32)).ord$c};f(0,0)=\"\\n\";f(m,n)=m?\"  \"++n?\"_/\"\n\
      \{- {- | -} -}n?x=do{[1..n];x}                                    --- obfuscated\n\
      \{-\\_/ on Fairbairn, with apologies to Chris Brown. Above is / Haskell 98 -}"
      ==>
      ["?", "f", "main"]

    , "showComplexFloat :: Double -> Double -> String\n\
      \showComplexFloat x 0.0 = showFFloat Nothing x \"\"\n\
      \showComplexFloat 0.0 y = showFFloat Nothing y \"i\"\n\
      \showComplexFloat x y = (showFFloat Nothing x \"\") ++ (if y > 0 then \"+\" else \"\") ++ (showFFloat Nothing y \"i\")"
      ==>
      ["showComplexFloat"]

    , "createAlert            :<|>\n\
      \ getAlert              :<|>\n\
      \ deleteAlert           :<|>\n\
      \ setAlertStatus        :<|>\n\
      \ tagAlert              :<|>\n\
      \ untagAlert            :<|>\n\
      \ updateAlertAttributes = client (Proxy :: Proxy AlertApi)"
      ==>
      ["createAlert"]

    , "_g :: X -> Y" ==> ["_g"]
    , "(f . g) x = f (g x)" ==> ["."]
    , "(#) :: TransArray c => c -> (b -> IO r) -> Trans c b -> IO r\n\
      \a # b = apply a b\n\
      \{-# INLINE (#) #-}\n\
      \\n"
      ==>
      ["#"]

    , "escape :: FilePath -> FilePath\n\
      \escape s = \"\\\"\" ++ concatMap esc s ++ \"\\\"\"\n\
      \  where\n\
      \  esc c | c `elem` ['\\\\', '\"']   = '\\\\' : [c]\n\
      \        | isAscii c && isPrint c = [c]\n\
      \        | otherwise              = \"\\\\x\" ++ showHex (fromEnum c) \"\\\\ \"\n\
      \\n\
      \------------------------------------------------------------------------\n\
      \-- Compiling Emacs Lisp files\n\
      \\n\
      \-- | The Agda mode's Emacs Lisp files, given in the order in which\n\
      \-- they should be compiled.\n\
      \\n\
      \emacsLispFiles :: [FilePath]\n\
      \emacsLispFiles =\n\
      \  [ \"agda2-abbrevs.el\"\n\
      \  , \"annotation.el\"\n\
      \  , \"agda2-queue.el\"\n\
      \  , \"eri.el\"\n\
      \  , \"agda2.el\"\n\
      \  , \"agda-input.el\"\n\
      \  , \"agda2-highlight.el\"\n\
      \  , \"agda2-mode.el\"\n\
      \  ]\n"
      ==>
      ["emacsLispFiles", "escape"]

    , "happyError = \tks i -> error (\n\
      \\t\"Parse error in line \" ++ show (i::Int) ++ \"\\n\")\n"
      ==>
      ["happyError"]
    , "> happyError = \tks i -> error (\n\
      \>\t\"Parse error in line \" ++ show (i::Int) ++ \"\\n\")\n"
      |=>
      ["happyError"]

    , "module UnindentedImportList(\n\
      \foo,\n\
      \(++),\n\
      \bar\
      \,\n\
      \quux \n\
      \)\n\
      \where\n\
      \\n\
      \import Test\n\
      \\n\
      \baz :: a -> a\n\
      \baz x = x\n\
      \"
      ==>
      ["UnindentedImportList", "baz"]

    , "\n\
      \\n\
      \#define FOO\n\
      \\n\
      \\n\
      \  module UnindentedImportList(\n\
      \  foo,\n\
      \  (++),\n\
      \  bar\
      \  ,\n\
      \  quux \n\
      \  )\n\
      \  where\n\
      \\n\
      \  import Test\n\
      \\n\
      \  baz :: a -> a\n\
      \  baz x = x\n\
      \"
      ==>
      ["FOO", "UnindentedImportList", "baz"]

    , toplevelFunctionsWithoutSignatures
    ]
    where
    (==>) = testTagNames filename
    (|=>) = testTagNames literateFilename
    toplevelFunctionsWithoutSignatures =
        testGroup "toplevel functions without signatures"
        [ "$(return . map sumDeclaration $ [0..15])" ==> []
        , "$( fmap (reverse . concat) . traverse prismsForSingleType $ [1..15] )" ==> []
        , "T.makeInstances [2..6]\nx" ==> []
        , "infix 5 |+|"  ==> []
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
        , "(x :+: y) *: z = x"     ==> ["*:"]
        , "((:+:) x y) *: z = x"   ==> ["*:"]
        , "(*:) (x :+: y) z = x"   ==> ["*:"]
        , "(*:) ((:+:) x y) z = x" ==> ["*:"]
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
        , "pattern x () = x + x"                        ==> ["pattern"]
        , "pattern, foo :: Int -> String -> [Int]"      ==> ["foo", "pattern"]
        , "foo, pattern :: Int -> String -> [Int]"      ==> ["foo", "pattern"]
        , "foo, pattern, bar :: Int -> String -> [Int]" ==> ["bar", "foo", "pattern"]
        , "pattern x = x "                              ==> ["pattern"]
        , "pattern x y = x + y"                         ==> ["pattern"]
        , "x `pattern` y = x + y"                       ==> ["pattern"]
        , "pattern x y z = x + y + z"                   ==> ["pattern"]
        -- Arguments named "forall
        , "f forall = forall + 1"                       ==> ["f"]
        , "a # b = apply a b"                           ==> ["#"]
        ]
    strictMatchTests = testGroup "strict match (!)"
        [ "f !x y = x"                ==> ["f"]
        , "f x !y = x"                ==> ["f"]
        , "f !x !y = x"               ==> ["f"]
        , "f ! x y = x"               ==> ["f"]
        -- this one is a bit controversial but it seems to be the way ghc
        -- parses it
        , "f ! x = x"                 ==> ["f"]
        , "(*:) !(x :+: y) z = x"    ==> ["*:"]
        , "(*:) !(!x :+: !y) !z = x" ==> ["*:"]
        , "(*:) !((:+:) x y) z = x"  ==> ["*:"]
        , "(*:) !((:+:) !x !y) !z = x" ==> ["*:"]
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
        , "(*:) ~(x :+: y) z = x" ==> ["*:"]
        , "(*:) ~(~x :+: ~y) ~z = x" ==> ["*:"]
        , "(*:) ~((:+:) x y) z = x" ==> ["*:"]
        , "(*:) ~((:+:) ~x ~y) ~z = x" ==> ["*:"]
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

        , "(*:) zzz@(x :+: y) z = x"             ==> ["*:"]
        , "(*:) zzz@(zx@x :+: zy@y) zz@z = x"    ==> ["*:"]
        , "(*:) zzz@((:+:) x y) z = x"           ==> ["*:"]
        , "(*:) zzz@((:+:) zs@x zs@y) zz@z = x"  ==> ["*:"]

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
    , "class A f where {\n\
      \  data F f :: * ; -- foo\n\
      \                  -- bar\n\
      \                  -- baz\n\
      \  mkF  :: f -> F f ; getF :: F f -> f ;\n\
      \} ;"
      ==>
      ["A", "F", "getF", "mkF"]
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
testLiterate = testGroup "Literate"
    [ "> class (X x) => C a b where\n\
      \>\tm :: a->b\n\
      \>\tn :: c\n"
    ==>
      ["C", "m", "n"]
    , "Test\n\
      \\\begin{code}\n\
      \class (X x) => C a b where\n\
      \\tm :: a->b\n\
      \\tn :: c\n\
      \\\end{code}"
      ==>
      ["C", "m", "n"]
    , "> precalcClosure0 :: Grammar -> Name -> RuleList\n\
      \> precalcClosure0 g = \n\
      \>\t\\n -> case lookup n info' of\n\
      \>\t\tNothing -> []\n\
      \>\t\tJust c  -> c\n\
      \>  where"
      ==>
      ["precalcClosure0"]
    , "New Resolutions by Jean-Luc Ponty, Scott O'Neil, and John Garvin\r\n\
      \\r\n\
      \> module Euterpea.Examples.NewResolutions where\r\n\
      \> import Euterpea\r\n\
      \\r\n\
      \> nrContext = Context {cTime = 0,\r\n\
      \>                      cPlayer = fancyPlayer,\r\n\
      \>                      cInst = Marimba,\r\n\
      \>                      cDur = 1.0,\r\n\
      \>                      cPch = 0,\r\n\
      \>                      cKey = (C,Major),\r\n\
      \>                      cVol = 100}\r\n\
      \>\r\n\
      \> tNewRes m = makeMidi (m, nrContext, defUpm)\r\n\
      \\r\n\
      \> root, minThird, fifth, octave :: Pitch -> Dur -> Music Pitch\r\n\
      \> root       p dur = Prim $ Note dur p\r\n\
      \> minThird   p dur = Prim $ Note dur (trans 3 p)\r\n\
      \> majThird   p dur = Prim $ Note dur (trans 4 p)\r\n\
      \> fifth      p dur = Prim $ Note dur (trans 7 p)\r\n\
      \> majSixth   p dur = Prim $ Note dur (trans 9 p)\r\n\
      \> minSeventh p dur = Prim $ Note dur (trans 10 p)\r\n\
      \> octave     p dur = Prim $ Note dur (trans 12 p)\r\n\
      \> oMinThird  p dur = Prim $ Note dur (trans 15 p)\r\n\
      \> oFifth     p dur = Prim $ Note dur (trans 19 p)"
      ==>
      [ "NewResolutions"
      , "fifth"
      , "majSixth"
      , "majThird"
      , "minSeventh"
      , "minThird"
      , "nrContext"
      , "oFifth"
      , "oMinThird"
      , "octave"
      , "root"
      , "tNewRes"
      ]
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

testDefine :: TestTree
testDefine = testGroup "preprocessor defines"
    [ "#define FOO 1" ==>
        ["FOO"]
    , "#define FOO \n\
      \ 1" ==>
        ["FOO"]
    , "#define FOO \n\
      \1" ==>
        ["FOO"]
    , "#define FOO(x) (x + x)" ==>
        ["FOO"]
    , "#if X\n\
      \#define FOO 1\n\
      \#else\n\
      \#define FOO 2\n\
      \#endif" ==>
        ["FOO"]
    , "#if X\n\
      \\n\
      \#define FOO 1\n\
      \\n\
      \\n\
      \#else\n\
      \\n\
      \\n\
      \#define FOO 2\n\
      \\n\
      \#endif" ==>
        ["FOO"]
    , "#define FOO(x) (x + x)" ==>
        ["FOO"]
    , "#let BAR x y z = \"x + y + z\"" ==>
        ["BAR"]
    ]
    where
    (==>) = testTagNames filename

testHSC2HS :: TestTree
testHSC2HS = testGroup "hsc2hs"
    [ "#{enum ControlOp, ControlOp\n\
      \ , controlOpAdd    = EPOLL_CTL_ADD\n\
      \ , controlOpModify = EPOLL_CTL_MOD\n\
      \ , controlOpDelete = EPOLL_CTL_DEL\n\
      \ }"
      ==>
      [ "controlOpAdd", "controlOpDelete", "controlOpModify"
      ]
    , "#{\n\
      \enum ControlOp, ControlOp\n\
      \ , controlOpAdd    = EPOLL_CTL_ADD\n\
      \ , controlOpModify = EPOLL_CTL_MOD\n\
      \ , controlOpDelete = EPOLL_CTL_DEL\n\
      \ }"
      ==>
      [ "controlOpAdd", "controlOpDelete", "controlOpModify"
      ]
    , "#{enum Test1, Test2 \n\
      \ , foo\n\
      \ , foo_bar\n\
      \ , BAR_BAZ\n\
      \ , BAR_BAZquux\n\
      \ }"
      ==>
      [ "barBaz", "barBazquux", "foo", "fooBar"
      ]
    , "#enum Mask, UserSpace, IN_ACCESS, IN_MODIFY, IN_ATTRIB, IN_CLOSE_WRITE\n\
      \#enum Mask, UserSpace, IN_CLOSE_NOWRITE, IN_OPEN, IN_MOVED_FROM, IN_MOVED_TO\n\
      \#enum Mask, UserSpace, IN_CREATE, IN_DELETE, IN_DELETE_SELF, IN_MOVE_SELF\n"
      ==>
      [ "inAccess", "inAttrib", "inCloseNowrite", "inCloseWrite"
      , "inCreate", "inDelete", "inDeleteSelf", "inModify"
      , "inMoveSelf", "inMovedFrom", "inMovedTo", "inOpen"
      ]

    , "#enum ExecOption,ExecOption, \\\n\
      \  execAnchored = PCRE_ANCHORED, \\\n\
      \  execNotBOL = PCRE_NOTBOL, \\\n\
      \  execNotEOL = PCRE_NOTEOL, \\\n\
      \  execNotEmpty = PCRE_NOTEMPTY, \\\n\
      \  execNoUTF8Check = PCRE_NO_UTF8_CHECK, \\\n\
      \  execPartial = PCRE_PARTIAL"
      ==>
      [ "execAnchored"
      , "execNoUTF8Check"
      , "execNotBOL"
      , "execNotEOL"
      , "execNotEmpty"
      , "execPartial"
      ]

    , "#enum ReturnCode,ReturnCode, \\\n\
      \  retNoMatch = PCRE_ERROR_NOMATCH, \\\n\
      \  retNull = PCRE_ERROR_NULL, \\\n\
      \  retBadOption = PCRE_ERROR_BADOPTION, \\\n\
      \  retBadMagic = PCRE_ERROR_BADMAGIC, \\\n\
      \  retUnknownNode = PCRE_ERROR_UNKNOWN_NODE, \\\n\
      \  retNoMemory = PCRE_ERROR_NOMEMORY, \\\n\
      \  retNoSubstring = PCRE_ERROR_NOSUBSTRING"
      ==>
      [ "retBadMagic"
      , "retBadOption"
      , "retNoMatch"
      , "retNoMemory"
      , "retNoSubstring"
      , "retNull"
      , "retUnknownNode"
      ]

    , "#{\n\
      \define hsc_patsyn(l, typ, cons, hprefix, recmac) { \\\n\
      \  struct { const char *s; unsigned n; } *p, list[] = { LLVM_HS_FOR_EACH_ ## l(recmac) }; \\\n\
      \  for(p = list; p < list + sizeof(list)/sizeof(list[0]); ++p) { \\\n\
      \    hsc_printf(\"pattern \" #hprefix \"%s :: \" #typ \"\\n\", p->s); \\\n\
      \    hsc_printf(\"pattern \" #hprefix \"%s =  \" #cons \" %u\\n\", p->s, p->n); \\\n\
      \  }\\\n\
      \}\\\n\
      \}\\\n\
      \\n\
      \foo x = x"
      ==>
      [ "foo"
      ]
    ]
    where
    (==>) = testTagNames "Test.hsc"

testAlex :: TestTree
testAlex = testGroup "alex"
    [ testGroup "vanilla"
        [ "{\n\
          \module AlexTest where\n\
          \\n\
          \import FooBar\n\
          \\n\
          \foobar :: Int -> Int\n\
          \foobar = (+ 1)\n\
          \\n\
          \}\n\
          \\n\
          \\n\
          \-- Can skip whitespace everywhere since it does not affect meaning in any\n\
          \-- state.\n\
          \<0, comment, qq, literate> $ws+ ;\n\
          \\n\
          \-- Literate Haskell support. 'literate' code handles all text except actual\n\
          \-- Haskell program text. It aims to strip all non-Haskell text.\n\
          \<literate> {\n\
          \$nl \">\" $ws*\n\
          \  { \\_ len -> (Newline $! len - 2) <$ startLiterateBird }\n\
          \$nl \"\\begin{code}\" @nl $space*\n\
          \  { \\input len -> (Newline $! countInputSpace input len) <$ startLiterateLatex }\n\
          \$nl ;\n\
          \.\n\
          \  { \\_ _ -> dropUntilNL' }\n\
          \}\n\
          \\n\
          \-- Vanilla tokens\n\
          \<0> {\n\
          \\n\
          \\"#\" @cpp_opt_ws (\"{\" (@cpp_ws | @nl)*)? \"enum\"\n\
          \  { \\_ _ -> pure HSCEnum }\n\
          \\"#\" @cpp_opt_ws\n\
          \  ( ($ascident # [e]) $ascident*\n\
          \  | $ascident ($ascident # [n]) $ascident*\n\
          \  | $ascident $ascident ($ascident # [u]) $ascident*\n\
          \  | $ascident $ascident $ascident ($ascident # [m]) $ascident*\n\
          \  | $ascident $ascident $ascident $ascident $ascident+\n\
          \  )\n\
          \  { \\_ _ -> pure HSCDirective }\n\
          \\"#\" @cpp_opt_ws \"{\" (@cpp_ws | @nl)*\n\
          \  ( ($ascident # [e]) $ascident*\n\
          \  | $ascident ($ascident # [n]) $ascident*\n\
          \  | $ascident $ascident ($ascident # [u]) $ascident*\n\
          \  | $ascident $ascident $ascident ($ascident # [m]) $ascident*\n\
          \  | $ascident $ascident $ascident $ascident $ascident+\n\
          \  )\n\
          \  { \\_ _ -> pure HSCDirectiveBraced }\n\
          \}\n\
          \\n\
          \\n\
          \\n\
          \\n\
          \{\n\
          \foo :: Int -> Int\n\
          \foo x = x + x\n\
          \}\n\
          \\n"
          ==>
          ["AlexTest", "foo", "foobar"]
      ]
    , testGroup "literate"
        [ "Very useful description 1\n\
          \Very useful description 2\n\
          \> {\n\
          \> module AlexTest where\n\
          \>\n\
          \> import FooBar\n\
          \>\n\
          \> foobar :: Int -> Int\n\
          \> foobar = (+ 1)\n\
          \>\n\
          \> }\n\
          \\n\
          \Useful description 1\n\
          \Useful description 2\n\
          \Useful description 3\n\
          \Useful description 4\n\
          \\n\
          \> -- Can skip whitespace everywhere since it does not affect meaning in any\n\
          \> -- state.\n\
          \> <0, comment, qq, literate> $ws+ ;\n\
          \>\n\
          \> -- Literate Haskell support. 'literate' code handles all text except actual\n\
          \> -- Haskell program text. It aims to strip all non-Haskell text.\n\
          \> <literate> {\n\
          \> $nl \">\" $ws*\n\
          \>   { \\_ len -> (Newline $! len - 2) <$ startLiterateBird }\n\
          \> $nl \"\\begin{code}\" @nl $space*\n\
          \>   { \\input len -> (Newline $! countInputSpace input len) <$ startLiterateLatex }\n\
          \> $nl ;\n\
          \> .\n\
          \>   { \\_ _ -> dropUntilNL' }\n\
          \> }\n\
          \>\n\
          \Useful description 1\n\
          \Useful description 2\n\
          \Useful description 3\n\
          \Useful description 4\n\
          \> -- Vanilla tokens\n\
          \> <0> {\n\
          \>\n\
          \> \"#\" @cpp_opt_ws (\"{\" (@cpp_ws | @nl)*)? \"enum\"\n\
          \>   { \\_ _ -> pure HSCEnum }\n\
          \> \"#\" @cpp_opt_ws\n\
          \>   ( ($ascident # [e]) $ascident*\n\
          \>   | $ascident ($ascident # [n]) $ascident*\n\
          \>   | $ascident $ascident ($ascident # [u]) $ascident*\n\
          \>   | $ascident $ascident $ascident ($ascident # [m]) $ascident*\n\
          \>   | $ascident $ascident $ascident $ascident $ascident+\n\
          \>   )\n\
          \>   { \\_ _ -> pure HSCDirective }\n\
          \> \"#\" @cpp_opt_ws \"{\" (@cpp_ws | @nl)*\n\
          \>   ( ($ascident # [e]) $ascident*\n\
          \>   | $ascident ($ascident # [n]) $ascident*\n\
          \>   | $ascident $ascident ($ascident # [u]) $ascident*\n\
          \>   | $ascident $ascident $ascident ($ascident # [m]) $ascident*\n\
          \>   | $ascident $ascident $ascident $ascident $ascident+\n\
          \>   )\n\
          \>   { \\_ _ -> pure HSCDirectiveBraced }\n\
          \> }\n\
          \>\n\
          \>\n\
          \> {\n\
          \> foo :: Int -> Int\n\
          \> foo x = x + x\n\
          \> }\n\
          \>\n\
          \\n"
          |=>
          ["AlexTest", "foo", "foobar"]
        , "Very useful description 1\n\
          \Very useful description 2\n\
          \\\begin{code}\n\
          \{\n\
          \module AlexTest where\n\
          \\n\
          \import FooBar\n\
          \\n\
          \foobar :: Int -> Int\n\
          \foobar = (+ 1)\n\
          \\n\
          \}\n\
          \\\end{code}\n\
          \\n\
          \Useful description 1\n\
          \Useful description 2\n\
          \Useful description 3\n\
          \Useful description 4\n\
          \\n\
          \\\begin{code}\n\
          \-- Can skip whitespace everywhere since it does not affect meaning in any\n\
          \-- state.\n\
          \<0, comment, qq, literate> $ws+ ;\n\
          \\n\
          \-- Literate Haskell support. 'literate' code handles all text except actual\n\
          \-- Haskell program text. It aims to strip all non-Haskell text.\n\
          \<literate> {\n\
          \$nl \">\" $ws*\n\
          \  { \\_ len -> (Newline $! len - 2) <$ startLiterateBird }\n\
          \$nl \"\\begin{code}\" @nl $space*\n\
          \  { \\input len -> (Newline $! countInputSpace input len) <$ startLiterateLatex }\n\
          \$nl ;\n\
          \.\n\
          \  { \\_ _ -> dropUntilNL' }\n\
          \}\n\
          \\n\
          \\\end{code}\n\
          \\n\
          \Useful description 1\n\
          \Useful description 2\n\
          \Useful description 3\n\
          \Useful description 4\n\
          \\n\
          \\\begin{code}\n\
          \-- Vanilla tokens\n\
          \<0> {\n\
          \\n\
          \\"#\" @cpp_opt_ws (\"{\" (@cpp_ws | @nl)*)? \"enum\"\n\
          \  { \\_ _ -> pure HSCEnum }\n\
          \\"#\" @cpp_opt_ws\n\
          \  ( ($ascident # [e]) $ascident*\n\
          \  | $ascident ($ascident # [n]) $ascident*\n\
          \  | $ascident $ascident ($ascident # [u]) $ascident*\n\
          \  | $ascident $ascident $ascident ($ascident # [m]) $ascident*\n\
          \  | $ascident $ascident $ascident $ascident $ascident+\n\
          \  )\n\
          \  { \\_ _ -> pure HSCDirective }\n\
          \\"#\" @cpp_opt_ws \"{\" (@cpp_ws | @nl)*\n\
          \  ( ($ascident # [e]) $ascident*\n\
          \  | $ascident ($ascident # [n]) $ascident*\n\
          \  | $ascident $ascident ($ascident # [u]) $ascident*\n\
          \  | $ascident $ascident $ascident ($ascident # [m]) $ascident*\n\
          \  | $ascident $ascident $ascident $ascident $ascident+\n\
          \  )\n\
          \  { \\_ _ -> pure HSCDirectiveBraced }\n\
          \}\n\
          \\n\
          \\n\
          \{\n\
          \foo :: Int -> Int\n\
          \foo x = x + x\n\
          \}\n\
          \\n\
          \\\end{code}\n\
          \\n"
          |=>
          ["AlexTest", "foo", "foobar"]
      ]
    ]
    where
    (==>) = testTagNames "Test.x"
    (|=>) = testTagNames "Test.lx"

testHappy :: TestTree
testHappy = testGroup "happy"
    [ testGroup "vanilla"
        [ "{\n\
          \{-# OPTIONS_GHC -w #-}\n\
          \module AttrGrammarParser (agParser) where\n\
          \import ParseMonad\n\
          \import AttrGrammar\n\
          \}\n\
          \\n\
          \%name agParser\n\
          \%tokentype { AgToken }\n\
          \%token\n\
          \  \"{\"     { AgTok_LBrace }\n\
          \  \"}\"     { AgTok_RBrace }\n\
          \  \";\"     { AgTok_Semicolon }\n\
          \  '{'       { AgTok_LBrace }\n\
          \  '}'       { AgTok_RBrace }\n\
          \  '::'      { AgTok_Semicolon }\n\
          \  \"=\"     { AgTok_Eq }\n\
          \  where     { AgTok_Where }\n\
          \  selfRef   { AgTok_SelfRef _ }\n\
          \  subRef    { AgTok_SubRef _ }\n\
          \  rightRef  { AgTok_RightmostRef _ }\n\
          \  unknown   { AgTok_Unknown _ }\n\
          \\n\
          \%monad { P }\n\
          \%lexer { agLexer } { AgTok_EOF }\n\
          \\n\
          \%%\n\
          \\n\
          \agParser :: { [AgRule] }\n\
          \  : rules                                      { $1 }\n\
          \\n\
          \rules :: { [AgRule] }\n\
          \  : rule '::' rules                            { $1 : $3 }\n\
          \  | rule                                       { $1 : [] }\n\
          \  |                                            { [] }\n\
          \\n\
          \rule :: { AgRule }\n\
          \  : selfRef  \"=\" code                        { SelfAssign (selfRefVal $1) $3 }\n\
          \  | subRef   \"=\" code                        { SubAssign (subRefVal $1) $3 }\n\
          \  | rightRef \"=\" code                        { RightmostAssign (rightRefVal $1) $3 }\n\
          \  | where code                                 { Conditional $2 }\n\
          \\n\
          \code :: { [AgToken] }\n\
          \  : '{' code0 '}' code                         { [$1] ++ $2 ++ [$3] ++ $4 }\n\
          \  | \"=\" code                                 { $1 : $2 }\n\
          \  | selfRef code                               { $1 : $2 }\n\
          \  | subRef code                                { $1 : $2 }\n\
          \  | rightRef code                              { $1 : $2 }\n\
          \  | unknown code                               { $1 : $2 }\n\
          \  |                                            { [] }\n\
          \\n\
          \code0 :: { [AgToken] }\n\
          \  : \"{\" code0 \"}\" code0                    { [$1] ++ $2 ++ [$3] ++ $4 }\n\
          \  | \"=\" code0                                { $1 : $2 }\n\
          \  | '::' code0                                 { $1 : $2 }\n\
          \  | selfRef code0                              { $1 : $2 }\n\
          \  | subRef code0                               { $1 : $2 }\n\
          \  | rightRef code                              { $1 : $2 }\n\
          \  | unknown code0                              { $1 : $2 }\n\
          \  |                                            { [] }\n\
          \\n\
          \{\n\
          \happyError :: P a\n\
          \happyError = fail (\"Parse error\\n\")\n\
          \\n\
          \test :: a -> a\n\
          \test x = x\n\
          \}\n\
          \\n"
          ==>
          ["AttrGrammarParser", "happyError", "test"]
        ]
    , testGroup "literate"
        [ "This parser parses the contents of the attribute grammar\n\
          \into a list of rules.  A rule can either be an assignment\n\
          \to an attribute of the LHS (synthesized attribute), and\n\
          \assignment to an attribute of the RHS (an inherited attribute),\n\
          \or a conditional statement.\n\
          \\n\
          \> {\n\
          \> {-# OPTIONS_GHC -w #-}\n\
          \> module AttrGrammarParser (agParser) where\n\
          \> import ParseMonad\n\
          \> import AttrGrammar\n\
          \> }\n\
          \\n\
          \> %name agParser\n\
          \> %tokentype { AgToken }\n\
          \> %token\n\
          \>   \"{\"     { AgTok_LBrace }\n\
          \>   \"}\"     { AgTok_RBrace }\n\
          \>   \";\"     { AgTok_Semicolon }\n\
          \>   \"=\"     { AgTok_Eq }\n\
          \>   where     { AgTok_Where }\n\
          \>   selfRef   { AgTok_SelfRef _ }\n\
          \>   subRef    { AgTok_SubRef _ }\n\
          \>   rightRef  { AgTok_RightmostRef _ }\n\
          \>   unknown   { AgTok_Unknown _ }\n\
          \>\n\
          \> %monad { P }\n\
          \> %lexer { agLexer } { AgTok_EOF }\n\
          \\n\
          \> %%\n\
          \\n\
          \> agParser :: { [AgRule] }\n\
          \>   : rules                                      { $1 }\n\
          \\n\
          \> rules :: { [AgRule] }\n\
          \>   : rule \";\" rules                           { $1 : $3 }\n\
          \>   | rule                                       { $1 : [] }\n\
          \>   |                                            { [] }\n\
          \\n\
          \> rule :: { AgRule }\n\
          \>   : selfRef  \"=\" code                        { SelfAssign (selfRefVal $1) $3 }\n\
          \>   | subRef   \"=\" code                        { SubAssign (subRefVal $1) $3 }\n\
          \>   | rightRef \"=\" code                        { RightmostAssign (rightRefVal $1) $3 }\n\
          \>   | where code                                 { Conditional $2 }\n\
          \\n\
          \> code :: { [AgToken] }\n\
          \>   : \"{\" code0 \"}\" code                     { [$1] ++ $2 ++ [$3] ++ $4 }\n\
          \>   | \"=\" code                                 { $1 : $2 }\n\
          \>   | selfRef code                               { $1 : $2 }\n\
          \>   | subRef code                                { $1 : $2 }\n\
          \>   | rightRef code                              { $1 : $2 }\n\
          \>   | unknown code                               { $1 : $2 }\n\
          \>   |                                            { [] }\n\
          \\n\
          \> code0 :: { [AgToken] }\n\
          \>   : \"{\" code0 \"}\" code0                    { [$1] ++ $2 ++ [$3] ++ $4 }\n\
          \>   | \"=\" code0                                { $1 : $2 }\n\
          \>   | \";\" code0                                { $1 : $2 }\n\
          \>   | selfRef code0                              { $1 : $2 }\n\
          \>   | subRef code0                               { $1 : $2 }\n\
          \>   | rightRef code                              { $1 : $2 }\n\
          \>   | unknown code0                              { $1 : $2 }\n\
          \>   |                                            { [] }\n\
          \\n\
          \> {\n\
          \> happyError :: P a\n\
          \> happyError = fail (\"Parse error\\n\")\n\
          \>\n\
          \> test :: a -> a\n\
          \> test x = x\n\
          \> }\n\
          \\n"
          |=>
          ["AttrGrammarParser", "happyError", "test"]
        , "This parser parses the contents of the attribute grammar\n\
          \into a list of rules.  A rule can either be an assignment\n\
          \to an attribute of the LHS (synthesized attribute), and\n\
          \assignment to an attribute of the RHS (an inherited attribute),\n\
          \or a conditional statement.\n\
          \\n\
          \\\begin{code}\n\
          \\n\
          \{\n\
          \{-# OPTIONS_GHC -w #-}\n\
          \module AttrGrammarParser (agParser) where\n\
          \import ParseMonad\n\
          \import AttrGrammar\n\
          \}\n\
          \\n\
          \\\end{code}\n\
          \\n\
          \\\begin{code}\n\
          \%name agParser\n\
          \%tokentype { AgToken }\n\
          \%token\n\
          \  \"{\"     { AgTok_LBrace }\n\
          \  \"}\"     { AgTok_RBrace }\n\
          \  \";\"     { AgTok_Semicolon }\n\
          \  \"=\"     { AgTok_Eq }\n\
          \  where     { AgTok_Where }\n\
          \  selfRef   { AgTok_SelfRef _ }\n\
          \  subRef    { AgTok_SubRef _ }\n\
          \  rightRef  { AgTok_RightmostRef _ }\n\
          \  unknown   { AgTok_Unknown _ }\n\
          \\n\
          \%monad { P }\n\
          \%lexer { agLexer } { AgTok_EOF }\n\
          \\\end{code}\n\
          \\n\
          \\\begin{code}\n\
          \%%\n\
          \\\end{code}\n\
          \\n\
          \\\begin{code}\n\
          \agParser :: { [AgRule] }\n\
          \  : rules                                      { $1 }\n\
          \\\end{code}\n\
          \\n\
          \\\begin{code}\n\
          \rules :: { [AgRule] }\n\
          \  : rule \";\" rules                           { $1 : $3 }\n\
          \  | rule                                       { $1 : [] }\n\
          \  |                                            { [] }\n\
          \\\end{code}\n\
          \\n\
          \\\begin{code}\n\
          \rule :: { AgRule }\n\
          \  : selfRef  \"=\" code                        { SelfAssign (selfRefVal $1) $3 }\n\
          \  | subRef   \"=\" code                        { SubAssign (subRefVal $1) $3 }\n\
          \  | rightRef \"=\" code                        { RightmostAssign (rightRefVal $1) $3 }\n\
          \  | where code                                 { Conditional $2 }\n\
          \\\end{code}\n\
          \\n\
          \\\begin{code}\n\
          \code :: { [AgToken] }\n\
          \  : \"{\" code0 \"}\" code                     { [$1] ++ $2 ++ [$3] ++ $4 }\n\
          \  | \"=\" code                                 { $1 : $2 }\n\
          \  | selfRef code                               { $1 : $2 }\n\
          \  | subRef code                                { $1 : $2 }\n\
          \  | rightRef code                              { $1 : $2 }\n\
          \  | unknown code                               { $1 : $2 }\n\
          \  |                                            { [] }\n\
          \\\end{code}\n\
          \\n\
          \\\begin{code}\n\
          \code0 :: { [AgToken] }\n\
          \  : \"{\" code0 \"}\" code0                    { [$1] ++ $2 ++ [$3] ++ $4 }\n\
          \  | \"=\" code0                                { $1 : $2 }\n\
          \  | \";\" code0                                { $1 : $2 }\n\
          \  | selfRef code0                              { $1 : $2 }\n\
          \  | subRef code0                               { $1 : $2 }\n\
          \  | rightRef code                              { $1 : $2 }\n\
          \  | unknown code0                              { $1 : $2 }\n\
          \  |                                            { [] }\n\
          \\\end{code}\n\
          \\n\
          \\\begin{code}\n\
          \{\n\
          \happyError :: P a\n\
          \happyError = fail (\"Parse error\\n\")\n\
          \\n\
          \test :: a -> a\n\
          \test x = x\n\
          \}\n\
          \\\end{code}\n\
          \\n"
          |=>
          ["AttrGrammarParser", "happyError", "test"]
        ]
    ]
    where
    (==>) = testTagNames "Test.y"
    (|=>) = testTagNames "Test.ly"


test :: (Show a, ExtendedEq b, Show b) => (a -> b) -> a -> b -> TestTree
test f x expected =
    HUnit.testCase (take 70 $ show x) $
    HUnit.assertBool msg (actual === expected)
    where
    actual = f x
    msg    = "expected: " ++ show expected ++ "\n but got: " ++ show actual

filename :: FilePath
filename = "fn.hs"

literateFilename :: FilePath
literateFilename = "fn.lhs"

testFullTagsWithPrefixes :: FilePath -> Text -> [Pos TagVal] -> TestTree
testFullTagsWithPrefixes fn = \source tags ->
    test (first List.sort . Tag.process fn trackPrefixes . TE.encodeUtf8) source (tags, warnings)
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
    process
        = first (List.sort . map untag)
        . Tag.process fn trackPrefixes
        . TE.encodeUtf8

    trackPrefixes :: Bool
    trackPrefixes = False

untag :: Pos TagVal -> String
untag (Pos _ (TagVal name _ _)) = T.unpack name

tokenize :: Text -> UnstrippedTokens
tokenize = tokenizeWith LitVanilla

tokenizeWith :: LitMode Void -> Text -> UnstrippedTokens
tokenizeWith mode =
    either (error . T.unpack) UnstrippedTokens .
    Lexer.tokenize filename mode trackPrefixes .
    TE.encodeUtf8
    where
    trackPrefixes = False

extractTokens :: UnstrippedTokens -> [Text]
extractTokens = map tokenName . Tag.unstrippedTokensOf
    where
    tokenName token = case Tag.valOf token of
        T name    -> name
        Newline n -> T.pack ("nl " ++ show n)
        t         -> T.pack $ show t
