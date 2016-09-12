{
-- alex doesn't quite produce warning-free code.
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-tabs
    -fno-warn-unused-binds -fno-warn-unused-matches
    -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lexer (tokenize) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Text (Text)

import LexerTypes
import Token

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

$special_sym  = [\(\)\,\;\[\]\`\{\}]
$ascsymbol    = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$unisymbol    = \x04
$symbol       = [$ascsymbol $unisymbol] # [$special_sym \_\'\"]

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
@forall      = ( "forall" | "∀" )

$charesc    = [a b f n r t v \\ \" \' \&]
$octdigit   = [0-9a-fA-F]
$hexdigit   = [0-9a-fA-F]
@charescape = [\\] ( $charesc | $asclarge+ | "o" $octdigit+ | "x" $hexdigit+ )

:-

-- Can skip whitespace everywhere since it does not affect meaning in any
-- state.
<0, comment, qq> $ws+ ;

<0> {

$nl $space*             { \_ len -> pure $ Newline $ len - 1 }
[\-][\-]+ ~[$symbol $nl] .* ;
[\-][\-]+ / $nl         ;

}

-- Comments
<0, comment> "{-"       { \_ _ -> startComment }
<comment> "-}"          { \_ _ -> endComment 0 }
<comment> (. | $nl)     ;
<0> "-}"                { \_ _ -> errorAtLine "Unmatched -}" }

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

<0> "[" $ident* "|"     { \_ _ -> startQuasiquoter }
<qq> "$("               { \_ _ -> startSplice CtxQuasiquoter }
<qq> "|]"               { \_ _ -> endQuasiquoter 0 }
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
@forall                 { kw KWForall }
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
"pattern"               { kw KWPattern }
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
-- semicolons are not used
";"                     ;

@qualificationPrefix ( $ident+ | $symbol+ )
                        { \input len -> return $ T $ retrieveToken input len }

}

{

type AlexAction m = AlexInput -> Int -> m TokenVal

kw :: (Applicative m) => TokenVal -> AlexAction m
kw tok = \_ _ -> pure tok

tokenize :: FilePath -> Bool -> Text -> Either String [Token]
tokenize filename trackPrefixes input =
    runAlexM filename trackPrefixes input scanTokens

scanTokens :: AlexM [Token]
scanTokens = do
    tok <- alexMonadScan
    case valOf tok of
        EOF -> return []
        _   -> (tok :) <$> scanTokens

alexMonadScan :: AlexM Token
alexMonadScan = do
    tokVal <- alexScanTokenVal
    -- Use input after reading toking to get proper prefix that includes
    -- token we currently read.
    AlexState {asInput, asFilename} <- get
    return $ Pos (mkSrcPos asFilename asInput) tokVal

alexScanTokenVal :: AlexM TokenVal
alexScanTokenVal = do
    AlexState {asInput, asCode} <- get
    case alexScan asInput asCode of
        AlexEOF                        ->
            return EOF
        AlexError (AlexInput {aiLine, aiInput}) -> do
            AlexState {asCode} <- get
            throwError $ "lexical error while in state " ++ show asCode ++ " at line " ++
                show (unLine aiLine) ++ ": " ++ take 40 (show aiInput)
        AlexSkip input _ ->
            alexSetInput input >> alexScanTokenVal
        AlexToken input tokLen action ->
            alexSetInput input >> action asInput tokLen

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

startQuasiquoter :: AlexM TokenVal
startQuasiquoter = do
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
popRParen _ _ = (tryRestoringContext *> return RParen) <|> return RParen

tryRestoringContext :: AlexM ()
tryRestoringContext = do
    ctx <- popContext
    alexSetStartCode $ case ctx of
        CtxHaskell     -> 0
        CtxQuasiquoter -> qq

errorAtLine :: (MonadError String m, MonadState AlexState m) => String -> m a
errorAtLine msg = do
    line <- gets (unLine . aiLine . asInput)
    throwError $ show line ++ ": " ++ msg

}
