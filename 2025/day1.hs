rotateDial :: Integer -> Integer
rotateDial n = n `mod` 100

-- Parse a line
parseInstruction :: String -> Integer
parseInstruction ('L' : rst) = -(read rst :: Integer)
parseInstruction ('R' : rst) = read rst :: Integer

readSequence :: [String] -> [Integer]
readSequence [s] = [parseInstruction s]
readSequence (s : sx) = parseInstruction s : readSequence sx

addIfZero :: Integer -> Integer
addIfZero n
  | n == 0 = 1
  | otherwise = 0

addZeros :: Integer -> Integer -> Integer
addZeros x y = abs ((x - y) `div` 100)

part1 :: Integer -> [Integer] -> Integer
part1 n [x] = (addIfZero . rotateDial) n
part1 n (x : xs) =
  let cursor = rotateDial (x + n)
   in addIfZero cursor + part1 cursor xs

-- FIXME: Wrong answer, investigate why
part2 :: Integer -> [Integer] -> Integer
part2 n [x] = rotateDial n
part2 n (x : xs) =
  let cursor = rotateDial (x + n)
   in addZeros x n + part2 cursor xs

main :: IO ()
main = do
  content <- readFile "2025/inputs/01.txt"
  print (part1 50 $ readSequence $ lines content)
