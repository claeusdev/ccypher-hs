module Caesar
  ( letterToInt,
    intToLetter,
    shift,
    encode,
    decode,
    percent,
    count,
    lowers,
    frequencyTable,
    chisqr,
    rotate,
  )
where

import Data.Char

letterToInt :: Char -> Int
letterToInt c = ord c - ord 'a'

intToLetter :: Int -> Char
intToLetter n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = intToLetter ((letterToInt c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n ws = [shift n x | x <- ws]

decode :: Int -> String -> String
decode n ws = [shift (-n) x | x <- ws]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

count :: Char -> String -> Int
count x xs = sum [1 | x' <- xs, x' == x]

lowers :: String -> Int
lowers xs = sum [1 | x <- xs, isLower x]

frequencyTable :: String -> [Float]
frequencyTable xs = [percent (count x xs) n | x <- ['a' .. 'z']] where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> String -> String
rotate n xs = drop n xs ++ take n xs
