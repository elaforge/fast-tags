{
-- alex doesn't quite produce warning-free code.
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-tabs
    -fno-warn-unused-binds -fno-warn-unused-matches
    -fno-warn-unused-imports #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE BangPatterns #-}

module FastTags.Lexer (tokenize) where

import Control.Applicative
import Control.Monad
#if MIN_VERSION_mtl(2,2,0)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif
import Control.Monad.State.Strict
import qualified Data.IntSet as IS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Unsafe as Text.Unsafe

import FastTags.LexerTypes
import FastTags.Token

}

$ascspace  = [\ \t\r]
$unispace  = \x01
$space     = [$ascspace $unispace]
$nl        = [\n]
$ws        = [$space\f\v] # $nl

$dot       = [\.]

$asclarge  = [A-Z]
$unilarge  = \x02
$large     = [$asclarge $unilarge]

$ascsmall  = [a-z]
$unismall  = \x03
$small     = [$ascsmall $unismall]

-- These symbols can be part of operators but are reserved when occur by
-- themselves.
$symbols_reserved_as_standalone = [ \→ \∷ \⇒ \∀ ]

$special_sym  = [\(\)\,\;\[\]\`\{\}]
$ascsymbol    = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$unisymbol    = \x04
$symbol       = [$ascsymbol $unisymbol $symbols_reserved_as_standalone] # [$special_sym \_\'\"]

$ascident  = [$ascsmall $asclarge]
$uniident  = [$unismall $unilarge]
$ascdigit  = [0-9]
$unidigit  = \x05
$digit     = [$ascdigit $unidigit]
$unisuffix = \x06
$ident_nonsym = [$ascident $uniident $unisuffix $digit] # [$symbol]
$ident_syms   = [\'\_\#]
$ident     = [$ident_nonsym $ident_syms]

-- $reserved_op = [→ ∷ ⇒ ∀]

@qualificationPrefix = ( $large $ident* $dot )*

@arrow       = ( "->" | "→" )
@doublecolon = ( "::" | "∷" )
@implies     = ( "=>" | "⇒" )

$charesc    = [a b f n r t v \\ \" \' \&]
$octdigit   = [0-7]
$hexdigit   = [0-9a-fA-F]
@charescape = [\\] ( $charesc | $asclarge+ | "o" $octdigit+ | "x" $hexdigit+ )

@float_number =  ( [\+\-]? ( $digit+ ( "." $digit+ )? | $digit* "." $digit+ ) ( [eE] [\+\-]? $digit* )? )

@number = ( [\+\-]? $digit+ | 0 ([oO] $octdigit+ | [xX] $hexdigit ) | @float_number )

:-

-- Can skip whitespace everywhere since it does not affect meaning in any
-- state.
<0, comment, qq> $ws+ ;

<0> {

[\\]? $nl $space* "{-"  { \input len -> startIndentationCounting (countInputSpace input len) }
[\\]? $nl $space*       { \input len -> pure $ Newline $! (len - 1) - (if Text.Unsafe.unsafeHead (aiInput input) == '\\' then 1 else 0) }
[\-][\-]+ ~[$symbol $nl] .* ;
[\-][\-]+ / $nl         ;

}

-- Comments
<0, comment>
  "{-"                  { \_ _ -> startComment }
<comment> "-}"          { \_ _ -> endComment 0 }
<comment> (. | $nl)     ;
<0> "-}"                { \_ _ -> errorAtLine "Unmatched -}" }

<indentComment>
  "{-"                  { \_ _ -> startIndentComment }
<indentComment> "-}"    { \_ _ -> endComment indentCount }
<indentComment> (. | $nl) ;

<indentCount> {
$space* "{-"            { \input len -> addIndentationSize (countInputSpace input len) *> startIndentComment }
$space*                 { \input len -> endIndentationCounting len }
}

-- Strings
<0> [\"]                { \_ _ -> startString }
<string> [\"]           { \_ _ -> endString 0 }
<string> [\\] $nl ( $ws+ [\\] )? ;
<string> ( $ws+ | [^\"\\$nl] | [\\] . )+ ;

-- Strings
<0> [\"]                { \_ _ -> startString }
<string> [\\] [\"\\]    ;
<string> [\\] $nl ($ws+ [\\])? ;
<string> [\"]           { \_ _ -> endString 0 }
<string> (. | $nl)      ;



-- Characters
<0> [\'] ( [^\'\\] | @charescape ) [\'] { kw Character }

-- Template Haskell quasiquoters

<0> "[" $ident* "|"     { \input len -> startQuasiquoter input len }
<0> "⟦"                 { \_ _ -> startUnconditionalQuasiQuoter }
<qq> "$("               { \_ _ -> startSplice CtxQuasiquoter }
<qq> ("|]" | "⟧")       { \_ _ -> endQuasiquoter 0 }
<qq> (. | $nl)          ;

<0> "$("                { \_ _ -> startSplice CtxHaskell }

-- Vanilla tokens
<0> {

"case"                  { kw KWCase }
"class"                 { kw KWClass }
"data"                  { kw KWData }
"default"               { kw KWDefault }
"deriving"              { kw KWDeriving }
"do"                    { kw KWDo }
"else"                  { kw KWElse }
"family"                { kw KWFamily }
"forall"                { \_ _ -> return $ T "forall" }
"∀"                     { \_ _ -> return $ T "forall" }
"foreign"               { kw KWForeign }
"if"                    { kw KWIf }
"import"                { kw KWImport }
"in"                    { kw KWIn }
"infix"                 { kw KWInfix }
"infixl"                { kw KWInfixl }
"infixr"                { kw KWInfixr }
"instance"              { kw KWInstance }
"let"                   { kw KWLet }
"module"                { kw KWModule }
"newtype"               { kw KWNewtype }
"of"                    { kw KWOf }
"pattern"               { \_ _ -> return $ T "pattern" }
"then"                  { kw KWThen }
"type"                  { kw KWType }
"where"                 { kw KWWhere }
@arrow                  { kw Arrow }
"@"                     { kw At }
"`"                     { kw Backtick }
","                     { kw Comma }
"."                     { kw Dot }
@doublecolon            { kw DoubleColon }
"="                     { kw Equals }
"!"                     { kw ExclamationMark }
@implies                { kw Implies }
"{"                     { kw LBrace }
"["                     { kw LBracket }
"("                     { pushLParen }
"|"                     { kw Pipe }
"}"                     { kw RBrace }
"]"                     { kw RBracket }
")"                     { popRParen }
"~"                     { kw Tilde }
";"                     { kw Semicolon }

-- Not interested in numbers, but it takes time to extract their text so
-- it's quicker to just ignore them.
@number                 { kw Number }

@qualificationPrefix ( $ident+ | $symbol+ )
                        { \input len -> return $ T $ retrieveToken input len }

}

{

type AlexAction m = AlexInput -> Int -> m TokenVal

kw :: (Applicative m) => TokenVal -> AlexAction m
kw tok = \_ _ -> pure tok

tokenize :: FilePath -> Bool -> Text -> Either String [Token]
tokenize filename trackPrefixes input =
    runAlexM trackPrefixes input $ scanTokens filename

scanTokens :: FilePath -> AlexM [Token]
scanTokens filename = go []
    where
    go acc = do
        nextTok <- alexScanTokenVal
        case nextTok of
            EOF -> return $ reverse acc
            _   -> do
                -- Use input after reading token to get proper prefix that includes
                -- token we currently read.
                input <- gets asInput
                let tok = Pos (mkSrcPos filename input) nextTok
                go (tok : acc)

alexScanTokenVal :: AlexM TokenVal
alexScanTokenVal = do
    AlexState {asInput, asCode} <- get
    go asInput asCode
    where
    go input code =
        case alexScan input code of
            AlexEOF ->
                return EOF
            AlexError (AlexInput {aiLine, aiInput}) -> do
                code <- gets asCode
                throwError $ "lexical error while in state " ++ show code
                    ++ " at line " ++
                    show (unLine aiLine) ++ ": " ++ take 40 (show aiInput)
            AlexSkip input' _ ->
                go input' code
            AlexToken input' tokLen action ->
                alexSetInput input' >> action input tokLen

startIndentationCounting :: Int -> AlexM TokenVal
startIndentationCounting !n = do
    modify (\s -> s { asIndentationSize = n, asCommentDepth = 1 })
    alexSetStartCode indentComment
    alexScanTokenVal

endIndentationCounting :: Int -> AlexM TokenVal
endIndentationCounting !n = do
    addIndentationSize n
    alexSetStartCode 0
    Newline <$> gets asIndentationSize

startIndentComment :: AlexM TokenVal
startIndentComment = do
    void $ modifyCommentDepth (+1)
    alexSetStartCode indentComment
    alexScanTokenVal

startComment :: AlexM TokenVal
startComment = do
    void $ modifyCommentDepth (+1)
    alexSetStartCode comment
    alexScanTokenVal

endComment :: Int -> AlexM TokenVal
endComment nextStartCode = do
    newDepth <- modifyCommentDepth (\x -> x - 1)
    when (newDepth == 0) $
        alexSetStartCode nextStartCode
    alexScanTokenVal

startString :: AlexM TokenVal
startString = do
    alexSetStartCode string
    alexScanTokenVal

endString :: Int -> AlexM TokenVal
endString nextStartCode = do
    alexSetStartCode nextStartCode
    return String

startQuasiquoter :: AlexInput -> Int -> AlexM TokenVal
startQuasiquoter (AlexInput {aiInput, aiAbsPos}) n
    | n == 2 = startUnconditionalQuasiQuoter
startQuasiquoter (AlexInput {aiInput, aiAbsPos}) _ = do
    ends   <- gets asPositionsOfQuasiQuoteEnds
    qqEnds <- case ends of
        Nothing    -> do
            let ends' = calculateQuasiQuoteEnds aiAbsPos aiInput
            modify $ \s -> s { asPositionsOfQuasiQuoteEnds = Just ends' }
            pure ends'
        Just ends' -> pure ends'
    case IS.lookupGT aiAbsPos qqEnds of
        -- No chance of quasi-quote closing till the end of current file.
        -- Assume that file ought to be well-formed and treat currently
        -- matched input
        Nothing -> return LBracket
        Just _  -> startUnconditionalQuasiQuoter

startUnconditionalQuasiQuoter :: AlexM TokenVal
startUnconditionalQuasiQuoter = do
    alexSetStartCode qq
    return QuasiquoterStart

startSplice :: Context -> AlexM TokenVal
startSplice ctx = do
    alexSetStartCode 0
    pushContext ctx
    return SpliceStart

endQuasiquoter :: Int -> AlexM TokenVal
endQuasiquoter nextStartCode = do
    alexSetStartCode nextStartCode
    return QuasiquoterEnd

pushLParen :: AlexAction AlexM
pushLParen _ _ = do
    pushContext CtxHaskell
    return LParen

popRParen :: AlexAction AlexM
popRParen _ _ = do
    cs <- gets asContextStack
    case cs of
        [] -> return ()
        c : cs' -> do
            modify $ \s -> s { asContextStack = cs' }
            alexSetStartCode $ case c of
                CtxHaskell     -> 0
                CtxQuasiquoter -> qq
    return RParen

errorAtLine :: (MonadError String m, MonadState AlexState m) => String -> m a
errorAtLine msg = do
    line <- gets (unLine . aiLine . asInput)
    throwError $ show line ++ ": " ++ msg

}
