{-# LANGUAGE OverloadedStrings #-}
-- | Functions specific to emacs tags.
module FastTags.Emacs (format) where
import qualified Data.Text as Text
import Data.Text (Text)

import qualified FastTags.Tag as Tag
import qualified FastTags.Token as Token
import qualified FastTags.Util as Util
import qualified FastTags.Vim as Vim


format :: Int -> [Token.Pos Tag.TagVal] -> [Text]
format maxSeparation = map (uncurry formatFileTags)
    . map (fmap (Vim.dropAdjacentInFile lineOf maxSeparation))
    . Util.groupOnKey (Token.posFile . Token.posOf)
    where lineOf = Token.unLine . Token.posLine . Token.posOf

formatFileTags :: FilePath -> [Token.Pos Tag.TagVal] -> Text
formatFileTags file tags = Text.concat
    [ "\x0c\x0a", Text.pack file, ","
    , Text.pack $ show (Text.length tagsText), "\x0a", tagsText
    ]
    where tagsText = Text.unlines $ map formatTag tags

formatTag :: Token.Pos Tag.TagVal -> Text
formatTag (Token.Pos pos (Tag.TagVal {})) = Text.concat
    [ Token.posPrefix pos
    , "\x7f"
    , Text.pack (show $ Token.unLine (Token.posLine pos))
    ]
