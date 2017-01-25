{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module FastTags.Tag (
    -- * types
    TagVal(..)
    , Type(..), toVimType, fromVimType
    , Tag(..)
    , Pos(..)
    , SrcPos(..)
    , UnstrippedTokens(..)
    -- * process
    , processFile
    , qualify
    , process
    -- * util
    , isHsFile
    , isLiterateFile

    -- TODO for testing
    , unstrippedTokensOf
    , stripCpp
    , stripNewlines
    , breakBlocks
    )
where
import Control.Arrow ((***))
import Control.DeepSeq (NFData, rnf)
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import Data.Functor ((<$>))
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Data.Monoid (Monoid, (<>))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error

import qualified Language.Preprocessor.Unlit as Unlit
import qualified System.FilePath as FilePath
import Text.Printf (printf)

import qualified FastTags.Lexer as Lexer
import qualified FastTags.Token as Token
import FastTags.Token (Pos(..), Token, SrcPos(..), TokenVal(..))
import qualified FastTags.Util as Util


-- * types

data TagVal = TagVal {
    tvName :: !Text
    , tvType :: !Type
    -- -- | If true, this tag will be marked static, which vim treats as an
    -- -- internal definition.
    -- , tvStatic :: !Bool
    } deriving (Show, Eq, Ord)

tagLine :: Pos TagVal -> Token.Line
tagLine = posLine . posOf

instance NFData TagVal where
    rnf (TagVal x y) = rnf x `seq` rnf y

-- | The Ord instance is used to sort tags with the same name.  Given multiple
-- matches, vim will visit them in order, so this should be in the order of
-- interest.
--
-- We rely that Type < Constructor.  TODO how and where?  For sorting tags?
data Type = Function | Type | Constructor | Class | Module | Operator | Pattern
    deriving (Eq, Ord, Show)

-- | Vim takes this to be the \"kind:\" annotation.  It's just an arbitrary
-- string and these letters conform to no standard.  Presumably there are some
-- vim extensions that can make use of it.
toVimType :: Type -> Char
toVimType typ = case typ of
    Module      -> 'm'
    Function    -> 'f'
    Class       -> 'c'
    Type        -> 't'
    Constructor -> 'C'
    Operator    -> 'o'
    Pattern     -> 'p'

fromVimType :: Char -> Maybe Type
fromVimType c = case c of
    'm' -> Just Module
    'f' -> Just Function
    'c' -> Just Class
    't' -> Just Type
    'C' -> Just Constructor
    'o' -> Just Operator
    'p' -> Just Pattern
    _ -> Nothing

instance NFData Type where
    rnf t = t `seq` ()

data Tag =
    Tag !(Pos TagVal)
    -- | Just like Tag, except these should be deduplicated by their TagVal,
    -- where the one with the lowest line number will be preferred.
    -- The idea seems to be that functions will emit a tag for both the
    -- signature and definition.  TODO seems like a hack, why not just
    -- deduplicate all tags?
    | RepeatableTag !(Pos TagVal)
    | Warning !(Pos String)
    deriving (Show, Eq, Ord)

-- | Partition Tag, RepeatableTag, and Warning.
partitionTags :: [Tag] -> ([Pos TagVal], [Pos TagVal], [Pos String])
partitionTags = go [] [] []
    where
    go tags repeats warns [] = (tags, repeats, reverse warns)
    go tags repeats warns (t:ts) = case t of
        Tag a           -> go (a:tags) repeats warns ts
        RepeatableTag a -> go tags (a:repeats) warns ts
        Warning a       -> go tags repeats (a:warns) ts

-- | Newlines have to remain in the tokens because 'breakBlocks' relies on
-- them.  But they make pattern matching on the tokens unreliable because
-- newlines might be anywhere.  A newtype makes sure that the tokens only get
-- stripped once and that I don't do any pattern matching on unstripped tokens.
newtype UnstrippedTokens = UnstrippedTokens [Token]
    deriving (Show, Monoid)

mapTokens :: ([Token] -> [Token]) -> UnstrippedTokens -> UnstrippedTokens
mapTokens f (UnstrippedTokens tokens) = UnstrippedTokens (f tokens)

unstrippedTokensOf :: UnstrippedTokens -> [Token]
unstrippedTokensOf (UnstrippedTokens tokens) = tokens

-- | Drop @n@ non-newline tokens.
dropTokens :: Int -> UnstrippedTokens -> UnstrippedTokens
dropTokens n = mapTokens (f n)
    where
    f :: Int -> [Token] -> [Token]
    f 0 xs                       = xs
    f _ []                       = []
    f n (Pos _ (Newline _) : xs) = f n xs
    f n (Pos _ _           : xs) = f (n - 1) xs

-- * processFile

-- | Read tags from one file.
processFile :: FilePath -> Bool -> IO ([Pos TagVal], [String])
processFile fn trackPrefixes = process fn trackPrefixes <$> readFileLenient fn

-- * qualify

-- | Each tag is split into a one qualified with its module name and one
-- without but marked Static.
qualify :: Pos TagVal -> Pos TagVal
qualify (Token.Pos pos (TagVal name typ)) =
    Token.Pos pos $ TagVal (T.pack module_ <> "." <> name) typ
    where
    module_ = FilePath.dropExtension $ FilePath.takeFileName $
        Token.posFile pos

-- | Read a UTF8 file, but don't crash on encoding errors.
readFileLenient :: FilePath -> IO Text
readFileLenient = fmap (Encoding.decodeUtf8With Encoding.Error.lenientDecode)
    . ByteString.readFile

-- | Process one file's worth of tags.
process :: FilePath -> Bool -> Text -> ([Pos TagVal], [String])
process fn trackPrefixes input =
    case Lexer.tokenize fn trackPrefixes $ stripCpp $ unlit input of
        Left msg -> ([], [msg])
        Right toks ->
            splitAndRemoveRepeats $
            concatMap blockTags $
            breakBlocks $
            UnstrippedTokens toks
    where
    splitAndRemoveRepeats :: [Tag] -> ([Pos TagVal], [String])
    splitAndRemoveRepeats tags =
        ( earliestRepeats ++ newTags
        , map valOf warnings
        )
        where
        (newTags, repeatableTags, warnings) = partitionTags tags
        -- For RepeatableTag s with duplicate keys, pick the one with the lowest
        -- posLine.
        -- Or: group by key, then map mininumOn tagLine
        earliestRepeats :: [Pos TagVal]
        earliestRepeats = Map.elems $ Map.fromListWith minLine $
            Util.keyOn valOf repeatableTags
        minLine x y
            | tagLine x < tagLine y = x
            | otherwise             = y
    unlit :: Text -> Text
    unlit src
        | isLiterateFile fn = T.pack $ Unlit.unlit fn $ T.unpack stripped
        | otherwise = src
        where
        stripped :: Text
        stripped
            | "\\begin{code}" `T.isInfixOf` src
                    && "\\end{code}" `T.isInfixOf` src =
                T.unlines $ filter (not . birdLiterateLine) $ T.lines src
            | otherwise = src
        birdLiterateLine :: Text -> Bool
        birdLiterateLine xs
            | T.null xs = False
            | otherwise = case Util.headt $ T.dropWhile Char.isSpace xs of
                Just '>' -> True
                _ -> False

-- | Strip cpp lines starting with #. Also strips out hsc detritus.
stripCpp :: Text -> Text
stripCpp =
    T.intercalate "\n" . snd . List.mapAccumL replaceCppLine False . T.lines
    where
    replaceCppLine :: Bool -> Text -> (Bool, Text)
    replaceCppLine insideMacro line
        | "#" `T.isPrefixOf` line = (insideMacro', T.empty)
        | insideMacro             = (insideMacro', T.empty)
        | otherwise               = (False, line)
        where
        insideMacro' = "\\" `T.isSuffixOf` line

startIdentChar :: Char -> Bool
startIdentChar c = Char.isAlpha c || c == '_'

identChar :: Bool -> Char -> Bool
identChar considerDot c = Char.isAlphaNum c || c == '\'' || c == '_'
    || c == '#' || considerDot && c == '.'

-- unicode operators are not supported yet
haskellOpChar :: Char -> Bool
haskellOpChar c = IntSet.member (Char.ord c) opChars
    where
    opChars :: IntSet.IntSet
    opChars = IntSet.fromList $ map Char.ord "-!#$%&*+./<=>?@^|~:\\"

isTypeVarStart :: Text -> Bool
isTypeVarStart x = case Util.headt x of
    Just c -> Char.isLower c || c == '_'
    _ -> False

-- | Break the input up into blocks based on indentation.
breakBlocks :: UnstrippedTokens -> [UnstrippedTokens]
breakBlocks =
    map UnstrippedTokens . filter (not . null)
        . go . filterBlank . unstrippedTokensOf
    where
    go :: [Token] -> [[Token]]
    go []     = []
    go tokens = pre : go post
        where (pre, post) = breakBlock tokens
    -- Blank lines mess up the indentation.
    filterBlank :: [Token] -> [Token]
    filterBlank [] = []
    filterBlank (Pos _ (Newline _) : xs@(Pos _ (Newline _) : _)) =
        filterBlank xs
    filterBlank (x:xs) = x : filterBlank xs

-- | Take until a newline, then take lines until the indent established after
-- that newline decreases. Or, alternatively, if "{" is encountered then count
-- it as a block until closing "}" is found taking nesting into account.
breakBlock :: [Token] -> ([Token], [Token])
breakBlock (t@(Pos _ tok) : ts) = case tok of
    Newline indent -> collectIndented indent ts
    LBrace         -> collectBracedBlock breakBlock ts 1
    _              -> remember t $ breakBlock ts
    where
    collectIndented :: Int -> [Token] -> ([Token], [Token])
    collectIndented indent tsFull@(t@(Pos _ tok) : ts) = case tok of
        Newline n | n <= indent -> ([], tsFull)
        LBrace ->
            remember t $ collectBracedBlock (collectIndented indent) ts 1
        _           -> remember t $ collectIndented indent ts
    collectIndented _ [] = ([], [])

    collectBracedBlock :: ([Token] -> ([Token], [Token])) -> [Token] -> Int
        -> ([Token], [Token])
    collectBracedBlock _    []                      _ = ([], [])
    collectBracedBlock cont ts                      0 = cont ts
    collectBracedBlock cont (t@(Pos _ LBrace) : ts) n =
      remember t $ collectBracedBlock cont ts $! n + 1
    collectBracedBlock cont (t@(Pos _ RBrace) : ts) n =
      remember t $ collectBracedBlock cont ts $! n - 1
    collectBracedBlock cont (t:ts)                  n =
      remember t $ collectBracedBlock cont ts n

    remember :: Token -> ([Token], [Token]) -> ([Token], [Token])
    remember t (xs, ys) = (t : xs, ys)
breakBlock [] = ([], [])

-- * extract tags

-- | Get all the tags in one indented block.
-- TODO clean this up to require less nesting, and dropDataContext duplication
blockTags :: UnstrippedTokens -> [Tag]
blockTags unstripped = case stripNewlines unstripped of
    [] -> []
    Pos _ KWModule : Pos pos (T name) : _ ->
          [mkTag pos (snd (T.breakOnEnd "." name)) Module]
    Pos _ KWPattern : Pos pos (T name) : _
        | maybe False Char.isUpper (Util.headt name) ->
            [mkTag pos name Pattern]
    Pos _ KWForeign : decl -> foreignTags decl
    -- newtype instance * = ...
    Pos _ KWNewtype : Pos _ KWInstance : (dropDataContext -> Pos pos _: rest) ->
        newtypeTags pos rest
    -- newtype X * = X *
    Pos prevPos KWNewtype : toks ->
        maybeToList tag ++ newtypeTags pos rest
        where
        (tag, pos, rest) =
            recordVanillaOrInfixName isTypeName Type prevPos "newtype * =" toks
    -- type family X ...
    Pos prevPos KWType : Pos _ KWFamily : toks -> maybeToList tag
        where
        (tag, _,  _) = recordVanillaOrInfixName isTypeFamilyName Type prevPos
            "type family * =" toks
    -- type instance X * = ...
    -- No tags in type family instances
    Pos _ KWType : Pos _ KWInstance : _ -> []
    -- type X * = ...
    Pos prevPos KWType : toks -> maybeToList tag
        where
        (tag, _, _) = recordVanillaOrInfixName isTypeName Type prevPos
            "type * =" toks
    -- data family X ...
    Pos prevPos KWData : Pos _ KWFamily : toks -> maybeToList tag
        where
        (tag, _, _) = recordVanillaOrInfixName isTypeFamilyName Type prevPos
            "data family * =" toks
    -- data instance * = ...
    -- data instance * where ...
    Pos _ KWData : Pos _ KWInstance : (dropDataContext -> Pos pos _: _) ->
        dataConstructorTags pos (dropTokens 2 unstripped)
    -- data X * = X { X :: *, X :: * }
    -- data X * where ...
    Pos prevPos KWData : toks ->
        maybeToList tag ++ dataConstructorTags pos (dropTokens 1 unstripped)
        where
        namePred = isTypeName -- isTypeFamilyName
        (tag, pos, _) = recordVanillaOrInfixName namePred Type prevPos
            "data * =" toks
    -- class * => X where X :: * ...
    Pos pos KWClass : _ -> classTags pos (dropTokens 1 unstripped)

    Pos _ KWInfix : _ -> []
    Pos _ KWInfixl : _ -> []
    Pos _ KWInfixr : _ -> []
    -- Deriving introduces no new names, just ignore it
    Pos _ KWDeriving : _ -> []
    -- instance * where data * = X :: * ...
    Pos pos KWInstance : _ ->
        instanceTags pos (dropTokens 1 unstripped)
    -- x, y, z :: *
    stripped -> toplevelFunctionTags stripped

isTypeFamilyName :: Text -> Bool
isTypeFamilyName =
    maybe False (\c -> Char.isUpper c || haskellOpChar c) . Util.headt

isTypeName  :: Text -> Bool
isTypeName x = case Util.headt x of
    Just c -> Char.isUpper c || c == ':'
    _ -> False

dropDataContext :: [Token] -> [Token]
dropDataContext = stripParensKindsTypeVars . stripOptContext

recordVanillaOrInfixName :: (Text -> Bool) -> Type -> SrcPos -> String
    -> [Token] -> (Maybe Tag, SrcPos, [Token])
recordVanillaOrInfixName isVanillaName tokenType prevPos context tokens =
    case dropDataContext tokens of
        Pos _ LParen   : Pos _ RParen : _ -> (Nothing, prevPos, tokens)
        Pos _ LBracket : _                -> (Nothing, prevPos, tokens)
        tok : toks ->
            case tok of
                Pos pos (T name) | isVanillaName name ->
                    (Just $ mkTag pos name tokenType, pos, toks)
                _ -> case dropInfixTypeStart $ tok : toks of
                    Pos pos (T name) : rest ->
                        (Just $ mkTag pos name tokenType, pos, rest)
                    rest -> (Just $ unexp pos rest, pos, tok : toks)
                        where pos = posOf tok
        [] -> (Just $ unexp prevPos [], prevPos, [])
    where
    unexp pos rest = unexpected pos (UnstrippedTokens tokens) rest context

-- same as dropWhile with counting
dropInfixTypeStart :: [Token] -> [Token]
dropInfixTypeStart tokens = dropWhile f tokens
    where
    f (Pos _ (T name)) = isInfixTypePrefix name
    f (Pos _ Backtick) = True
    f (Pos _ LParen)   = True
    f _                = False

    isInfixTypePrefix :: Text -> Bool
    isInfixTypePrefix = maybe False Char.isLower . Util.headt

-- | It's easier to scan for tokens without pesky newlines popping up
-- everywhere.  But I need to keep the newlines in in case I hit a @where@
-- and need to call 'breakBlocks' again.
stripNewlines :: UnstrippedTokens -> [Token]
stripNewlines = filter (not . isNewline) . (\(UnstrippedTokens t) -> t)

-- | Tags from foreign import.
--
-- e.g. @foreign import ccall safe \"name\" c_name :: ...@ will produce a tag
-- for @c_name@.
foreignTags :: [Token] -> [Tag]
foreignTags decl = case decl of
    Pos _ KWImport : decl'
        | Pos pos (T name) : _ <- Util.dropBefore isDoubleColon decl' ->
            [mkTag pos name Function]
    _ -> []
    where
    isDoubleColon (Pos _ DoubleColon) = True
    isDoubleColon _ = False

toplevelFunctionTags :: [Token] -> [Tag]
toplevelFunctionTags toks = case tags of
    -- Tags of toplevel functions are all repeatable, even the ones that come
    -- from the type signature because there will definitely be tags from the
    -- body and they should be sorted out if type signature is present.
    [] -> functionTagsNoSig toks
    _  -> map toRepeatableTag tags
    where
    -- first try to detect tags from type signature, if it fails then
    -- do the actual work of detecting from body
    (tags, _) = functionTags False toks
    toRepeatableTag :: Tag -> Tag
    toRepeatableTag (Tag t) = RepeatableTag t
    toRepeatableTag t       = t

functionTagsNoSig :: [Token] -> [Tag]
functionTagsNoSig toks = go toks
    where
    go :: [Token] -> [Tag]
    go []                           = []
    go toks@(Pos _ LParen : _)      = go $ stripBalancedParens toks
    -- This function does not analyze type signatures.
    go (Pos _ DoubleColon : _)      = []
    go (Pos _ ExclamationMark : ts) = go ts
    go (Pos _ Tilde : ts)           = go ts
    go (Pos _ At : ts)              = go ts
    go (Pos _ Equals : _)           = functionOrOp toks
    go (Pos _ Pipe : _)             = functionOrOp toks
    go toks@(Pos _ LBrace : _)      = go $ stripBalancedBraces toks
    go (Pos _ Backtick : Pos pos' (T name') : _)
        | functionName False name'  = [mkRepeatableTag pos' name' Function]
    go (Pos pos (T name) : _)
        | T.all haskellOpChar name  = [mkRepeatableTag pos name Operator]
    go (_ : ts)                     = go ts
    stripOpeningParens :: [Token] -> [Token]
    stripOpeningParens = dropWhile ((== LParen) . valOf)
    functionOrOp :: [Token] -> [Tag]
    functionOrOp toks = case stripOpeningParens toks of
         Pos pos (T name) : _
             | functionName False name -> [mkRepeatableTag pos name Function]
         Pos pos tok : _ -> case tokToOpName tok of
             Just name -> [mkRepeatableTag pos name Operator]
             Nothing   -> []
         [] -> []

tokToOpName :: TokenVal -> Maybe Text
tokToOpName (T name)
    | T.all haskellOpChar name = Just name
tokToOpName ExclamationMark = Just "!"
tokToOpName Tilde           = Just "~"
tokToOpName Dot             = Just "."
tokToOpName _               = Nothing

-- | Get tags from a function type declaration: token , token , token ::
-- Return the tokens left over.
functionTags :: Bool -- ^ expect constructors, not functions
    -> [Token] -> ([Tag], [Token])
functionTags constructors = go []
    where
    opTag   = if constructors then Constructor else Operator
    funcTag = if constructors then Constructor else Function
    go :: [Tag] -> [Token] -> ([Tag], [Token])
    go tags (Pos _ LParen : opTok : Pos _ RParen : Pos _ DoubleColon : rest) =
        (reverse $ mkOpTag tags opTag opTok, rest)
    go tags (Pos pos (T name) : Pos _ DoubleColon : rest)
        | functionName constructors name =
            (reverse $ mkTag pos name funcTag : tags, rest)
    go tags (Pos _ LParen : opTok : Pos _ RParen : Pos _ Comma : rest) =
        go (mkOpTag tags opTag opTok) rest
    go tags (Pos pos (T name) : Pos _ Comma : rest)
        | functionName constructors name =
            go (mkTag pos name funcTag : tags) rest
    go tags tokens = (tags, tokens)

    mkOpTag :: [Tag] -> Type -> Token -> [Tag]
    mkOpTag tags opTag (Pos pos tok) =
      case tokToOpName tok of
        Just name -> mkTag pos name opTag : tags
        Nothing   -> tags

functionName :: Bool -> Text -> Bool
functionName constructors text = isFunction text
    where
    isFunction text = case T.uncons text of
        Just (c, cs) ->
            firstChar c && startIdentChar c && T.all (identChar True) cs
        Nothing      -> False
    firstChar = if constructors
                then Char.isUpper
                else \c -> Char.isLower c || c == '_'

-- | * = X *
newtypeTags :: SrcPos -> [Token] -> [Tag]
newtypeTags prevPos tokens = case dropUntil Equals tokens of
    Pos pos (T name) : rest ->
        let constructor = mkTag pos name Constructor
        in  case rest of
            Pos _ LBrace : Pos funcPos (T funcName) : _ ->
                [constructor, mkTag funcPos funcName Function]
            _ ->
                [constructor]
    rest -> [unexpected prevPos (UnstrippedTokens tokens) rest "newtype * ="]

-- | [] (empty data declaration)
-- * = X { X :: *, X :: * }
-- * where X :: * X :: *
-- * = X | X
dataConstructorTags :: SrcPos -> UnstrippedTokens -> [Tag]
dataConstructorTags prevPos unstripped
    -- GADT
    | any (\case { Pos _ KWWhere -> True; _ -> False })
            (unstrippedTokensOf unstripped) =
        concatMap gadtTags (whereBlock unstripped)
    -- plain ADT
    | otherwise = case strip unstripped of
        [] -> [] -- empty data declaration
        rest | Just (Pos pos (T name), rest') <- extractInfixConstructor rest ->
            mkTag pos name Constructor : collectRest rest'
        Pos pos (T name) : rest ->
            mkTag pos name Constructor : collectRest rest
        Pos _ LParen : Pos pos (T name) : Pos _ RParen : rest ->
            mkTag pos name Constructor : collectRest rest
        rest -> [unexpected prevPos unstripped rest "data * = *"]
    where
    strip = stripOptBang . stripOptContext . stripOptForall . dropUntil Equals
         . stripNewlines
    collectRest :: [Token] -> [Tag]
    collectRest tokens
        | (tags@(_:_), rest) <- functionTags False tokens =
            tags ++ collectRest (dropUntilNextField rest)
    collectRest (Pos pipePos Pipe : rest)
        | Just (Pos pos (T name), rest'') <- extractInfixConstructor rest' =
            mkTag pos name Constructor : collectRest rest''
        | Pos pos (T name) : rest'' <- rest' =
            mkTag pos name Constructor
                : collectRest (dropUntilNextCaseOrRecordStart rest'')
        | Pos _ LParen : Pos pos (T name) : Pos _ RParen : rest'' <- rest' =
            mkTag pos name Constructor
                : collectRest (dropUntilNextCaseOrRecordStart rest'')
        | otherwise = error $
            printf "syntax error@%d: | not followed by tokens\n"
                (Token.unLine $ posLine pipePos)
        where
        rest' = stripOptBang $ stripOptContext $ stripOptForall rest
    collectRest (_ : rest) = collectRest rest
    collectRest [] = []


    stripOptBang :: [Token] -> [Token]
    stripOptBang ((Pos _ ExclamationMark) : rest) = rest
    stripOptBang ts = ts

    extractInfixConstructor :: [Token] -> Maybe (Token, [Token])
    extractInfixConstructor = extract . stripTypeParam
        where
        extract :: [Token] -> Maybe (Token, [Token])
        extract (tok@(Pos _ (T name)) : rest)
            | ":" `T.isPrefixOf` name = Just (tok, stripTypeParam rest)
        extract (Pos _ Backtick : tok@(Pos _ _) : Pos _ Backtick : rest) =
            Just (tok, stripTypeParam rest)
        extract _ = Nothing

        stripTypeParam :: [Token] -> [Token]
        stripTypeParam input@((Pos _ LParen) : _) =
            stripBalancedParens input
        stripTypeParam input@((Pos _ LBracket) : _) =
            stripBalancedBrackets input
        stripTypeParam ts = drop 1 ts

    dropUntilNextCaseOrRecordStart :: [Token] -> [Token]
    dropUntilNextCaseOrRecordStart = dropWithStrippingBalanced $
        not . \case { Pipe -> True; LBrace -> True; _ -> False }

    dropUntilNextField :: [Token] -> [Token]
    dropUntilNextField = dropWithStrippingBalanced $
        not . \case { Comma -> True; RBrace -> True; Pipe -> True; _ -> False }

stripOptForall :: [Token] -> [Token]
stripOptForall (Pos _ KWForall  : rest) = dropUntil Dot rest
stripOptForall xs                       = xs

stripParensKindsTypeVars :: [Token] -> [Token]
stripParensKindsTypeVars (Pos _ LParen : xs)  =
    stripParensKindsTypeVars xs
stripParensKindsTypeVars (Pos _ DoubleColon : xs) =
    stripParensKindsTypeVars $ drop 1 $
    dropWithStrippingBalanced (\case { RParen -> False; _ -> True }) xs
stripParensKindsTypeVars (Pos _ (T name) : xs)
    | isTypeVarStart name = stripParensKindsTypeVars xs
stripParensKindsTypeVars xs = xs

stripOptContext :: [Token] -> [Token]
stripOptContext (stripBalancedParens     -> Pos _ Implies : xs) = xs
stripOptContext (stripSingleClassContext -> Pos _ Implies : xs) = xs
stripOptContext xs                                              = xs

stripSingleClassContext :: [Token] -> [Token]
stripSingleClassContext (Pos _ (T name) : xs)
    | maybe False Char.isUpper (Util.headt name) =
        dropWithStrippingBalanced f xs
    where
    f (T name) = isTypeVarStart name
    f _        = False
stripSingleClassContext xs = xs

-- | Drop all tokens for which @pred@ returns True, also drop () or []
-- parenthesized expressions.
dropWithStrippingBalanced :: (TokenVal -> Bool) -> [Token] -> [Token]
dropWithStrippingBalanced pred input@(Pos _ LParen : _) =
    dropWithStrippingBalanced pred $ stripBalancedParens input
dropWithStrippingBalanced pred input@(Pos _ LBracket : _) =
    dropWithStrippingBalanced pred $ stripBalancedBrackets input
dropWithStrippingBalanced pred (Pos _ tok : xs)
    | pred tok  = dropWithStrippingBalanced pred xs
dropWithStrippingBalanced _ xs = xs

stripBalancedParens :: [Token] -> [Token]
stripBalancedParens = stripBalanced LParen RParen

stripBalancedBrackets :: [Token] -> [Token]
stripBalancedBrackets = stripBalanced LBracket RBracket

stripBalancedBraces :: [Token] -> [Token]
stripBalancedBraces = stripBalanced LBrace RBrace

stripBalanced :: TokenVal -> TokenVal -> [Token] -> [Token]
stripBalanced open close (Pos _ tok : xs)
    | tok == open = go 1 xs
    where
    go :: Int -> [Token] -> [Token]
    go 0 xs = xs
    go !n (Pos _ tok' : xs)
        | tok' == open  = go (n + 1) xs
        | tok' == close = go (n - 1) xs
    go n (_: xs) = go n xs
    go _ []      = []
stripBalanced _ _ xs = xs

gadtTags :: UnstrippedTokens -> [Tag]
gadtTags = fst . functionTags True . stripNewlines

-- | * => X where X :: * ...
classTags :: SrcPos -> UnstrippedTokens -> [Tag]
classTags prevPos unstripped =
    maybeToList tag ++ concatMap classBodyTags (whereBlock wherePart)
    where
    (classPart, wherePart) = spanUntil KWWhere unstripped
    (tag, _, _) = recordVanillaOrInfixName isTypeName Class prevPos
        "class * =>" $ stripUntilImplies $ stripNewlines classPart

stripUntilImplies :: [Token] -> [Token]
stripUntilImplies xs =
    case dropUntil Implies xs of
        []  -> xs
        xs' -> xs'

classBodyTags :: UnstrippedTokens -> [Tag]
classBodyTags unstripped = case stripNewlines unstripped of
    Pos _ KWType : Pos pos (T name) : _ -> [mkTag pos name Type]
    Pos _ KWData : Pos pos (T name) : _ -> [mkTag pos name Type]
    tokens                              -> fst $ functionTags False tokens

-- | Skip to the where and split the indented block below it.
whereBlock :: UnstrippedTokens -> [UnstrippedTokens]
whereBlock = breakBlocks . mapTokens (dropUntil KWWhere)

instanceTags :: SrcPos -> UnstrippedTokens -> [Tag]
instanceTags prevPos unstripped =
    -- instances can offer nothing but some fresh data constructors since
    -- the actual datatype is really declared in the class declaration
    concatMap (newtypeTags prevPos . stripNewlines)
        (filter isNewtypeDecl block)
    ++ concatMap (dataConstructorTags prevPos)
        (filter isDataDecl block)
    where
    block = whereBlock unstripped

    isNewtypeDecl :: UnstrippedTokens -> Bool
    isNewtypeDecl (UnstrippedTokens (Pos _ KWNewtype : _)) = True
    isNewtypeDecl _ = False

    isDataDecl :: UnstrippedTokens -> Bool
    isDataDecl (UnstrippedTokens (Pos _ KWData : _)) = True
    isDataDecl _ = False

-- * util

mkTag :: SrcPos -> Text -> Type -> Tag
mkTag pos name typ = Tag $ Pos pos (TagVal name typ)

mkRepeatableTag :: SrcPos -> Text -> Type -> Tag
mkRepeatableTag pos name typ = RepeatableTag $ Pos pos (TagVal name typ)

warning :: SrcPos -> String -> Tag
warning pos warn = Warning $ Pos pos $ show pos ++ ": " ++ warn

unexpected :: SrcPos -> UnstrippedTokens -> [Token] -> String -> Tag
unexpected prevPos (UnstrippedTokens tokensBefore) tokensHere declaration =
    warning pos ("unexpected " ++ thing ++ " after " ++ declaration)
    where
    thing = maybe "end of block" (show . valOf) (Util.mhead tokensHere)
    pos
        | Just t <- Util.mhead tokensHere = posOf t
        | Just t <- Util.mlast tokensBefore = posOf t
        | otherwise = prevPos

isNewline :: Token -> Bool
isNewline (Pos _ (Newline _)) = True
isNewline _                   = False

dropUntil :: TokenVal -> [Token] -> [Token]
dropUntil token = drop 1 . dropWhile (not . (== token) . valOf)

spanUntil :: TokenVal -> UnstrippedTokens
    -> (UnstrippedTokens, UnstrippedTokens)
spanUntil token =
    (UnstrippedTokens *** UnstrippedTokens)
    .  span (not . (== token) . valOf) . unstrippedTokensOf

-- | Crude predicate for Haskell files
isHsFile :: FilePath -> Bool
isHsFile = (`elem` [".hs", ".hsc", ".lhs"]) . FilePath.takeExtension

isLiterateFile :: FilePath -> Bool
isLiterateFile = (==".lhs") . FilePath.takeExtension
