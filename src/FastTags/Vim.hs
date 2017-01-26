{-# LANGUAGE OverloadedStrings #-}
-- | Functions specific to vim tags.
module FastTags.Vim where
import Control.Arrow (first)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Read as Text.Read

import qualified FastTags.Tag as Tag
import qualified FastTags.Token as Token
import qualified FastTags.Util as Util


-- | Format new tags, drop old tags from the loaded files, merge old and
-- new, and sort.
merge :: [FilePath] -> [[Token.Pos Tag.TagVal]] -> [Text] -> [Text]
merge fns new old = (vimMagicLine:) $
    map snd $ dropAdjacent maxSeparation $ Util.sortOn fst $ newTags ++ oldTags
    -- The existing vimMagicLine will fail parseTag and be dropped.
    where
    maxSeparation = 2
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
    concatMap (Util.sortOn fst . stripName) . Util.groupOn (name . fst)
    where
    -- Group by filename, sort by line number, drop lines too close.
    stripName tag@[_] = tag
    stripName tags = concatMap stripFile . Util.groupOn (filename . fst)
        . Util.sortOn (filename . fst) $ tags
    stripFile = stripLine . Util.sortOn (line . fst)
    stripLine [] = []
    stripLine ((tag, a) : tags) =
        (tag, a) : stripLine (dropWhile (tooClose tag) tags)
    tooClose tag = (<= line tag + maxSeparation) . line . fst

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
        <*> (Tag.fromVimType =<< Util.headt type_)
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
showTag (Token.Pos pos (Tag.TagVal text typ)) = mconcat
    [ text, "\t"
    , Text.pack (Token.posFile pos), "\t"
    , Text.pack (show $ Token.unLine (Token.posLine pos)), ";\"\t"
    , Text.singleton (Tag.toVimType typ)
    ]
