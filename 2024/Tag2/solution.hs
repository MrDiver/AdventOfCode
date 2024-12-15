import Data.List (sort, transpose)
import Foreign.Marshal.Utils (fromBool)
import Util (count)

readInt :: String -> Integer
readInt = read

tuplify2 [x, y] = (x, y)
tuplify2 _ = error "Empty Tuple is not valid"

listify2 (x, y) = [x, y]

fork :: (t -> a) -> (t -> b) -> t -> (a, b)
fork f g x = (f x, g x)

pairwiseDiff = map (uncurry (-) . tuplify2) . transpose . listify2 . fork tail init

-- check if list is monotone
checkStrictMonotony :: [Integer] -> Bool
checkStrictMonotony = uncurry (||) . fork (all (> 0)) (all (< 0)) . pairwiseDiff

-- All differences at least 1 at most 3
checkDifferences :: [Integer] -> Bool
checkDifferences = all (uncurry (&&) . fork (3 >=) (>= 1) . abs) . pairwiseDiff

checkReport :: [Integer] -> Integer
checkReport = fromBool . uncurry (&&) . fork checkDifferences checkStrictMonotony

-- Solution to Part 1
solution :: String -> String
solution = show . sum . map (checkReport . map readInt . words) . lines

-- Solution to Part 2

-- Removes element from a list at index n
dropNth :: Int -> [a] -> [a]
dropNth n = uncurry (++) . fork (take n) (drop (n + 1))

-- Generate a list of functions with length n each removing the nth element
listsMissingOneF :: Int -> [[a] -> [a]]
listsMissingOneF n = map dropNth [1 .. n]

-- generate a list of lists with each list having the nth element dropped from the original corresponding to its index
listsMissingOne :: [a] -> [[a]]
listsMissingOne xs = sequence (listsMissingOneF $ length xs) xs

solution2 :: String -> String
solution2 = show . sum . map (fromBool . elem 1 . map checkReport . listsMissingOne . map readInt . words) . lines

main :: IO ()
main = interact solution2