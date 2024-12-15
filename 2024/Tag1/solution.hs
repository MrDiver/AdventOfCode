import Data.List (transpose, sort)
import Foreign.Marshal.Utils (fromBool)

readInt :: String -> Integer
readInt = read

sortBoth :: [[Integer]] -> [[Integer]]
sortBoth = transpose . map sort . transpose

tuplify2 [x,y] = (x,y)
tuplify2 _ = error "Empty Tuple is not valid"

listify2 (x,y) = [x,y]

distances :: [[Integer]] -> [Integer]
distances = map (uncurry (-).tuplify2)

-- Solution to Part 1
solution :: String -> String
solution = show . sum . distances . sortBoth . map (map readInt . words) . lines

-- Everything part 2 related

countOccurence :: (Num a1, Eq a2) => [a2] -> a2 -> a1
countOccurence l x = sum $ map (fromBool . (==x)) l

mapToOccurencePairs :: (Num b, Eq a2) => [a2] -> [a2] -> ([a2], [b])
mapToOccurencePairs a b = (a, map (countOccurence b) a)

solution2 = show . sum . map (uncurry (*) . tuplify2) . transpose . listify2 . uncurry mapToOccurencePairs . tuplify2 . sort . transpose . map (map readInt . words) . lines

main :: IO ()
main = interact solution2