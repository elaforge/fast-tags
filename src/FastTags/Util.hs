-- | Generic utilities.
module FastTags.Util where
import qualified Data.Char as Char
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)


-- | Drop until the element before the matching one.  Return [] if the function
-- never matches.
dropBefore :: (a -> Bool) -> [a] -> [a]
dropBefore f = go
    where
    go [] = []
    go [_] = []
    go xs@(_ : rest@(y:_))
        | f y = xs
        | otherwise = go rest

sortOn :: (Ord k) => (a -> k) -> [a] -> [a]
sortOn key = List.sortBy (compare `Function.on` key)

-- | Split list into chunks delimited by specified element.
split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split x xs = xs': split x (drop 1 xs'')
    where (xs', xs'') = break (==x) xs

headt :: Text -> Maybe Char
headt = fmap fst . Text.uncons

mhead :: [a] -> Maybe a
mhead [] = Nothing
mhead (x:_) = Just x

mlast :: [a] -> Maybe a
mlast xs
    | null xs = Nothing
    | otherwise = Just (last xs)

keyOn :: (a -> k) -> [a] -> [(k, a)]
keyOn f xs = zip (map f xs) xs

groupOn :: Eq k => (a -> k) -> [a] -> [[a]]
groupOn key = List.groupBy (\a b -> key a == key b)

-- | Group the unsorted list into @(key x, xs)@ where all @xs@ compare equal
-- after @key@ is applied to them.  List is returned in sorted order.
groupOnKey :: Ord key => (a -> key) -> [a] -> [(key, [a])]
groupOnKey key = Map.toAscList . List.foldl' go Map.empty
    where go m x = Map.alter (Just . maybe [x] (x:)) (key x) m

isSymbolCharacterCategory :: Char.GeneralCategory -> Bool
isSymbolCharacterCategory cat = Set.member cat symbolCategories
    where
    symbolCategories :: Set.Set Char.GeneralCategory
    symbolCategories = Set.fromList
        [ Char.ConnectorPunctuation
        , Char.DashPunctuation
        , Char.OtherPunctuation
        , Char.MathSymbol
        , Char.CurrencySymbol
        , Char.ModifierSymbol
        , Char.OtherSymbol
        ]
