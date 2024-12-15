import Data.List
import Foreign.Marshal.Utils (fromBool)
import Util (count)
import Data.Char (isDigit)

readInt :: String -> Integer
readInt = read

tuplify2 [x, y] = (x, y)
tuplify2 _ = error "Empty Tuple is not valid"

listify2 (x, y) = [x, y]

fork :: (t -> a) -> (t -> b) -> t -> (a, b)
fork f g x = (f x, g x)

-- indices str = filter (-1/=) . (\xs -> [if x then i else -1 | (i,x) <- zip [0..] xs]) . map (isPrefixOf str) . tails
starts str = filter (isPrefixOf str) . tails

data Op a b = Mul a b | Nop | Do | Dont
    deriving Show

notNop Nop = False
notNop _ = True

applyOp (Mul a b) = a * b
applyOp _ = 0

parseMul xs = if x0 == "mul" && x1 == "(" && x2 == "," && ")" `isPrefixOf` x3 && not (null n1) && not (null n2) then Mul (readInt n1) (readInt n2) else Nop
    where
       x0 = take 3 xs
       r0 = drop 3 xs
       (x1,r1) = span ('('==) r0
       (n1,r2) = span isDigit r1
       (x2,r3) = span (','==) r2
       (n2,r4) = span isDigit r3
       (x3,r5) = span (')'==) r4



-- Solution to Part 1
solution :: String -> String
solution = show . sum . map applyOp . filter notNop . map parseMul . tails

-- Solution to Part 2

parse xs
    | "mul" `isPrefixOf` xs = parseMul xs
    | "do()" `isPrefixOf` xs = Do
    | "don't()" `isPrefixOf` xs = Dont
    | otherwise = Nop

processOps :: [Op a b] -> [Op a b]
processOps xs = intern xs True
    where
        intern :: [Op a b] -> Bool -> [Op a b]
        intern (Do:xs) _ = intern xs True
        intern (Dont:xs) _ = intern xs False
        intern (x:xs) enabled = if enabled then x:intern xs enabled else intern xs enabled
        intern [] enabled = []

solution2 :: String -> String
solution2 = show . sum . map applyOp . processOps . filter notNop . map parse . tails

main :: IO ()
main = interact solution2