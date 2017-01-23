-- | This is a fake module so ghci works.  When cabal builds, it will be
-- shadowed by Lexer.x output.  TODO get ghci to generate and use the alex
-- output.
module FastTags.Lexer where
import qualified Data.Text as Text

import qualified FastTags.Token as Token


tokenize :: FilePath -> Bool -> Text.Text -> Either String [Token.Token]
tokenize _ _ _ = Left "no lexer"
