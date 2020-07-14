import Data.Ratio

f :: Integer -> Rational -> Rational
f = (\a b -> recip (fromIntegral a + b))

fs :: [Integer]
fs = concat [ [ 1, 2 * k, 1 ] | k <- [ 1 .. ] ] :: [Integer]

ds :: [Rational]
ds = map (\x -> 2 + foldr f 0 (take x fs) ) [ 0 .. ]

n :: Integer
n = numerator (ds !! 99)

s :: Integer -> Integer
s 0 = 0
s x = let (n, d) = x `quotRem` 10 in d + s n

main = print (s n)

