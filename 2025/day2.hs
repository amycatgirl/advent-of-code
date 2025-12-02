import Data.Array (inRange)
import Data.Char (ord)

type Range = (Integer, Integer)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n == 0 || n < 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

isInvalid :: [Integer] -> Bool
isInvalid n =
  let len = length n `div` 2
      pattern = take len n
   in drop len n == pattern

-- syntax is as follows
-- f-l,f-l
-- where
-- f => first number of range
-- l => last number of range
extractNumber :: String -> String -> String
extractNumber [c] acc = acc ++ [c]
extractNumber (c : cx) acc
  | inRange (ord '0', ord '9') (ord c) = extractNumber cx (acc ++ [c])
  | otherwise = acc

parseNumber :: String -> Integer
parseNumber n = read $ extractNumber n ""

toRange :: [Integer] -> Range
toRange [f, l] = (f, l)

-- From https://lotz84.github.io/haskellbyexample/ex/string-functions
-- I copied this function because I really didn't want to make my own split function just yet
split :: String -> Char -> [String]
split "" _ = []
split xs c =
  let (ys, zs) = break (== c) xs
   in if null zs then [ys] else ys : split (tail zs) c

parseRange :: String -> Range
parseRange n = toRange $ map parseNumber $ split n '-'

parseEntireFile :: String -> [Range]
parseEntireFile f = map parseRange $ split f ','

leaveIf :: (a -> Bool) -> [a] -> [a]
leaveIf f [s]
  | f s = [s]
  | otherwise = []
leaveIf f (s : xs)
  | f s = s : leaveIf f xs
  | otherwise = leaveIf f xs

part1 :: [Range] -> Integer
part1 [(f, l)] = sum (leaveIf (isInvalid . toDigits) [f .. l])
part1 ((f, l) : xs) = sum (leaveIf (isInvalid . toDigits) [f .. l]) + part1 xs

main :: IO ()
main = do
  input <- readFile "2025/inputs/02.txt"
  print $ part1 $ parseEntireFile input
