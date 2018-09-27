{-# LANGUAGE OverloadedStrings #-}
-- | Functions specific to emacs tags.
module FastTags.Emacs (format) where
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)

import qualified FastTags.Tag as Tag
import qualified FastTags.Token as Token
import qualified FastTags.Util as Util
import qualified FastTags.Vim as Vim


format :: Int -> [Token.Pos Tag.TagVal] -> [Text]
format maxSeparation = map (uncurry formatFileTags)
    . map (fmap (dropAdjacent maxSeparation))
    . Util.groupOnKey (Token.posFile . Token.posOf)

-- | Like 'Vim.dropAdjacent', but since emacs isn't incremental it deals with
-- TagVals, not tag file lines.  Also the tags are already grouped by file.
dropAdjacent :: Int -> [Token.Pos Tag.TagVal] -> [Token.Pos Tag.TagVal]
dropAdjacent maxSeparation = concatMap dropInName. Util.groupOn nameOf
    where
    nameOf = Tag.tvName . Token.valOf
    lineOf = Token.unLine . Token.posLine . Token.posOf
    dropInName tag@[_] = tag
    dropInName tags = Vim.dropAdjacentInFile lineOf maxSeparation tags

formatFileTags :: FilePath -> [Token.Pos Tag.TagVal] -> Text
formatFileTags file tags = Text.concat
    [ "\x0c\n", Text.pack file, ","
    , showt (Text.length tagsText), "\n", tagsText
    ]
    where tagsText = Text.unlines $ map formatTag tags

-- https://en.wikipedia.org/wiki/Ctags#Etags_2
formatTag :: Token.Pos Tag.TagVal -> Text
formatTag (Token.Pos pos tag) = Text.concat
    [ text, "\x7f"
    , matching, "\x01"
    , linenum , ",", offset
    ]
    where
    text = (Token.posPrefix pos) <> (Token.posSuffix pos)
    matching = Tag.tvName tag
    linenum = showt $ Token.unLine (Token.posLine pos)
    offset = showt $ Token.unOffset (Token.posOffset pos) - (Text.length $ Token.posPrefix pos)

showt :: Show a => a -> Text
showt = Text.pack . show
