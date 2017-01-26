{-# LANGUAGE OverloadedStrings #-}
-- | Functions specific to emacs tags.
module FastTags.Emacs (format) where
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)

import qualified FastTags.Tag as Tag
import qualified FastTags.Token as Token
import qualified FastTags.Vim as Vim


type TagsTable = Map.Map FilePath [Token.Pos Tag.TagVal]

format :: Int -> [Token.Pos Tag.TagVal] -> [Text]
format maxSeparation = showTagsTable
    . fmap (Vim.dropAdjacentInFile lineOf maxSeparation)
    . foldr insertTag Map.empty
    where lineOf = Token.unLine . Token.posLine . Token.posOf

-- TODO this is just groupByKey
insertTag :: Token.Pos Tag.TagVal -> TagsTable -> TagsTable
insertTag tag@(Token.Pos pos _) = Map.insertWith (<>) (Token.posFile pos) [tag]

showTagsTable :: TagsTable -> [Text]
showTagsTable = map (uncurry showSection) . Map.toList

showSection :: FilePath -> [Token.Pos Tag.TagVal] -> Text
showSection file tags = Text.concat
    ["\x0c\x0a", Text.pack file, ","
    , Text.pack $ show tagsLength, "\x0a", tagsText
    ]
    where
    tagsText = Text.unlines $ map showEmacsTag tags
    tagsLength = Text.length tagsText

showEmacsTag :: Token.Pos Tag.TagVal -> Text
showEmacsTag (Token.Pos pos (Tag.TagVal {})) = Text.concat
    [ Token.posPrefix pos
    , "\x7f"
    , Text.pack (show $ Token.unLine (Token.posLine pos))
    ]
