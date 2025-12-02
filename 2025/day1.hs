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
addIfZero i
  | i == 0 = 1
  | otherwise = 0

part1 :: Integer -> [Integer] -> Integer
part1 n [x] = (addIfZero . rotateDial) n
part1 n (x : xs) =
  let cursor = rotateDial (x + n)
   in addIfZero cursor + part1 cursor xs

main :: IO ()
main = do
  content <- readFile "inputs/01.txt"
  print (part1 50 $ readSequence $ lines content)
