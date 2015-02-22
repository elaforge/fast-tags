{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions specific to vim tags.
module FastTags.Vim (
    merge, dropAdjacentInFile
    -- for tests
    , Parsed(..), parseTag, dropAdjacent, keyOnJust, showTag
) where
#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative
import Data.Monoid
#endif
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Read as Text.Read

import qualified FastTags.Tag as Tag
import qualified FastTags.Token as Token
import qualified FastTags.Util as Util


-- | Format new tags, drop old tags from the loaded files, merge old and
-- new, and sort.
merge :: Int -> [FilePath] -> [[Token.Pos Tag.TagVal]] -> [Text] -> [Text]
merge maxSeparation fns new old = (vimMagicLine:) $
    map snd $ dropAdjacent maxSeparation $ Util.sortOn fst $ newTags ++ oldTags
    -- The existing vimMagicLine will fail parseTag and be dropped.
    where
    newTags = keyOnJust parseTag $ map showTag (concat new)
    oldTags = filter ((`Set.notMember` fnSet) . filename . fst) $
        keyOnJust parseTag old
    fnSet = Set.fromList $ map Text.pack fns

keyOnJust :: (a -> Maybe k) -> [a] -> [(k, a)]
keyOnJust f xs = [(k, x) | (Just k, x) <- Util.keyOn f xs]

-- | If there are multiple tags with the same name and filename within a few
-- lines, drop all but the first.
dropAdjacent :: Int -> [(Parsed, a)] -> [(Parsed, a)]
dropAdjacent maxSeparation =
    concatMap (Util.sortOn fst . dropInName). Util.groupOn (name . fst)
    where
    -- Group by filename, sort by line number, drop lines too close.
    dropInName tag@[_] = tag
    dropInName tags = concatMap dropInFile . Util.groupOn (filename . fst)
        . Util.sortOn (filename . fst) $ tags
    dropInFile = dropAdjacentInFile (line . fst) maxSeparation

-- | Split this out so I can share it with emacs.
dropAdjacentInFile :: (a -> Int) -> Int -> [a] -> [a]
dropAdjacentInFile lineOf maxSeparation = stripLine . Util.sortOn lineOf
    where
    stripLine [] = []
    stripLine (tag : tags) =
        tag : stripLine (dropWhile (tooClose tag) tags)
    tooClose tag = (<= lineOf tag + maxSeparation) . lineOf


-- | The Ord instance determines the sort order for the tags file.
data Parsed = Parsed {
    name :: !Text
    , type_ :: !Tag.Type
    , filename :: !Text
    , line :: !Int
    } deriving (Eq, Ord, Show)

-- text <tab> fname <tab> line;" <tab> type
parseTag :: Text -> Maybe Parsed
parseTag t = case Text.split (=='\t') t of
    text : fname : line : type_ : _ -> Parsed
        <$> Just text
        <*> (fromType =<< Util.headt type_)
        <*> Just fname
        <*> either (const Nothing) (Just . fst) (Text.Read.decimal line)
    _ -> Nothing

-- | This line is to tell vim that the file is sorted, so it can use binary
-- search when looking for tags. This must come first in the tags file, and the
-- format is documented in :h tags-file-format as:
--
--   !_TAG_FILE_SORTED<Tab>1<Tab>{anything}
--
-- However, simply leaving {anything} part empty or putting something random
-- like ~ doesn't work when we want to extend the tags file with some tags from
-- C files using ctags. ctags requires //, with optional comments in between two
-- slashes. More about ctags' file format can be seen here:
-- http://ctags.sourceforge.net/FORMAT.
vimMagicLine :: Text
vimMagicLine = "!_TAG_FILE_SORTED\t1\t//"

-- | Convert a Tag to text, e.g.: AbsoluteMark\tCmd/TimeStep.hs 67 ;" f
showTag :: Token.Pos Tag.TagVal -> Text
showTag (Token.Pos pos (Tag.TagVal text typ _)) = mconcat
    [ text, "\t"
    , Text.pack (Token.posFile pos), "\t"
    , Text.pack (show $ Token.unLine (Token.posLine pos)), ";\"\t"
    , Text.singleton (toType typ)
    ]

-- | Vim takes this to be the \"kind:\" annotation.  It's just an arbitrary
-- string and these letters conform to no standard.  Presumably there are some
-- vim extensions that can make use of it.
toType :: Tag.Type -> Char
toType typ = case typ of
    Tag.Module      -> 'm'
    Tag.Function    -> 'f'
    Tag.Class       -> 'c'
    Tag.Type        -> 't'
    Tag.Constructor -> 'C'
    Tag.Operator    -> 'o'
    Tag.Pattern     -> 'p'
    Tag.Family      -> 'F'

fromType :: Char -> Maybe Tag.Type
fromType c = case c of
    'm' -> Just Tag.Module
    'f' -> Just Tag.Function
    'c' -> Just Tag.Class
    't' -> Just Tag.Type
    'C' -> Just Tag.Constructor
    'o' -> Just Tag.Operator
    'p' -> Just Tag.Pattern
    'F' -> Just Tag.Family
    _ -> Nothing
