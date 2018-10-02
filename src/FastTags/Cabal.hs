{-# LANGUAGE OverloadedStrings #-}
-- | Parse Cabal files.
module FastTags.Cabal where
import qualified Control.Monad as Monad
import Control.Monad ((<=<))
import Data.Bifunctor (first)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error

import qualified Distribution.Parsec.Parser as Parser


parse :: FilePath -> IO (Either String (FilePath, [ModuleName]))
parse fname =
    first ((fname <> ": ")<>) . (extract <=< first show . Parser.readFields) <$>
    ByteString.readFile fname

type ModuleName = Text.Text

{- | First I look for the library section, then get two things from it:

    > Section (Name _ "library") [] fields
    > Field (Name _ "hs-source-dirs") [FieldLine _ dir]
    > Field (Name _ "exposed-modules") [name | FieldLine _ name]
-}
extract :: Show ann => [Parser.Field ann]
    -> Either String (FilePath, [ModuleName])
extract parsed = do
    fields <- library parsed
    (,) <$> hsSourceDir fields <*> pure (exposed fields)
    where
    hsSourceDir fields = case find (isField "hs-source-dirs") fields of
        Just [Parser.FieldLine _ name] -> Right (ByteString.Char8.unpack name)
        Just [] -> Right "."
        Nothing -> Right "."
        Just fields -> Left $ "multiple hs-source-dirs: " <> show fields
    exposed = map get . Maybe.fromMaybe [] . find (isField "exposed-modules")
        where get (Parser.FieldLine _ name) = utf8 name
    isField name (Parser.Field (Parser.Name _ fieldName) lines)
        | caseEq fieldName name = Just lines
    isField _ _ = Nothing

    library = maybe (Left "no library stanza") Right . find isLibrary
    isLibrary (Parser.Section (Parser.Name _ name) [] fields)
        | caseEq name "library" = Just fields
    isLibrary _ = Nothing

find :: (a -> Maybe b) -> [a] -> Maybe b
find f = Monad.msum . map f

caseEq :: ByteString.ByteString -> Text.Text -> Bool
caseEq bytes text = Text.toLower (utf8 bytes) == text

utf8 :: ByteString.ByteString -> Text.Text
utf8 = Encoding.decodeUtf8With Encoding.Error.lenientDecode
