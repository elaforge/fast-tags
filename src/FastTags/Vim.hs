{-# LANGUAGE OverloadedStrings #-}
-- | Functions specific to vim tags.
module FastTags.Vim where
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)

import qualified FastTags.Tag as Tag
import qualified FastTags.Token as Token
import qualified FastTags.Util as Util


-- | Format new tags, drop old tags from the loaded files, merge old and
-- new, and sort.
merge :: [FilePath] -> [[Token.Pos Tag.TagVal]] -> [Text] -> [Text]
merge fns new old = map snd $ Util.sortOn fst $ newTags ++ oldTags
    where
    newTags = Util.keyOn parseTag $ map showTag (concat new)
    oldTags = filter (maybe True ((`Set.notMember` fnSet) . filename) . fst) $
        Util.keyOn parseTag old
    fnSet = Set.fromList $ map Text.pack fns

data Parsed = Parsed {
    name :: !Text
    , type_ :: !(Maybe Tag.Type)
    , filename :: !Text
    } deriving (Eq, Ord, Show)

-- text <tab> fname;" <tab> type
parseTag :: Text -> Maybe Parsed
parseTag t = case Text.split (=='\t') t of
    text : fname : type_ : _ -> Just $ Parsed
        { name = text
        , type_ = Tag.fromVimType =<< Util.headt type_
        , filename = Text.dropEnd 2 fname
        }
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
