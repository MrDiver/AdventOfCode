import Data.List (sort, transpose)
import Foreign.Marshal.Utils (fromBool)

readInt :: String -> Integer
readInt = read

sortBoth :: [[Integer]] -> [[Integer]]
sortBoth = transpose . map sort . transpose

tuple = (,) <$> head <*> head.tail

listify2 (x, y) = [x, y]

unlist f (a : b : _) = f a b
unlist f _ = error "No empty lists allowed"

-- Solution to Part 1
solution :: String -> String
solution = show . sum . map abs . unlist (zipWith (-)) . map sort . transpose . map (map readInt . words) . lines

-- Everything part 2 related

countOccurence :: (Num a1, Eq a2) => [a2] -> a2 -> a1
countOccurence l x = sum $ map (fromBool . (== x)) l

mapToOccurencePairs :: (Num b, Eq a2) => [a2] -> [a2] -> ([a2], [b])
mapToOccurencePairs a b = (a, map (countOccurence b) a)

solution2 = show . sum . map (uncurry (*) . tuple) . transpose . listify2 . uncurry mapToOccurencePairs . tuple . sort . transpose . map (map readInt . words) . lines

main :: IO ()
main = interact solution2