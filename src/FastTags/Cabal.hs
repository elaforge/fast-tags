{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Parse Cabal files.
module FastTags.Cabal (parse) where
import qualified Control.Monad as Monad
import Control.Monad ((<=<))
import Data.Bifunctor (first)
import qualified Data.ByteString as ByteString
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error

#if ! MIN_VERSION_Cabal(2, 2, 0)
parse :: FilePath -> IO (Either String (FilePath, [FilePath]))
parse = const $ return $ Left "cabal parsing not supported <Cabal-2.2.0"
#else

import qualified Distribution.Parsec.Parser as Parser


parse :: FilePath -> IO (Either String (FilePath, [FilePath]))
parse fname =
    first ((fname <> ": ")<>) . (extract <=< first show . Parser.readFields) <$>
    ByteString.readFile fname

{- | First I look for the library section, then get two things from it:

    > Section (Name _ "library") [] fields
    > Field (Name _ "hs-source-dirs") [FieldLine _ dir]
    > Field (Name _ "exposed-modules") [name | FieldLine _ name]
-}
extract :: Show ann => [Parser.Field ann]
    -> Either String (FilePath, [FilePath]) -- ^ (hsSrcDir, modulePath)
extract parsed = do
    fields <- library parsed
    (,) <$> hsSourceDir fields <*> pure (exposed fields)
    where
    hsSourceDir fields = case commaField (findField "hs-source-dirs" fields) of
        [] -> Right "."
        [dir] -> Right $ Text.unpack dir
        -- to support it, I'd have to search myself.  Too much bother.
        dir : _ -> Right $ Text.unpack dir
        -- dirs -> Left $ "multiple hs-source-dirs: " <> show dirs
    exposed = map moduleToFile . commaField . findField "exposed-modules"
    -- executable-only packages have no library section
    library = maybe (Right []) Right . find isLibrary
    isLibrary (Parser.Section (Parser.Name _ name) [] fields)
        | caseEq name "library" = Just fields
    isLibrary _ = Nothing

-- | exposed-modules might be comma separated.  It might not.  It might have
-- spaces.  Or it might not.  Who knows, it's cabal!
commaField :: Text.Text -> [Text.Text]
commaField = Text.words . Text.replace "," " "

moduleToFile :: Text.Text -> FilePath
moduleToFile = Text.unpack . (<>".hs") . Text.replace "." "/"

findField :: Text.Text -> [Parser.Field ann] -> Text.Text
findField name = Text.unwords . maybe [] (map get) . find (isField name)
    where
    get (Parser.FieldLine _ fieldName) = utf8 fieldName
    isField name (Parser.Field (Parser.Name _ fieldName) lines)
        | caseEq fieldName name = Just lines
    isField _ _ = Nothing

find :: (a -> Maybe b) -> [a] -> Maybe b
find f = Monad.msum . map f

caseEq :: ByteString.ByteString -> Text.Text -> Bool
caseEq bytes text = Text.toLower (utf8 bytes) == text

utf8 :: ByteString.ByteString -> Text.Text
utf8 = Encoding.decodeUtf8With Encoding.Error.lenientDecode

#endif
