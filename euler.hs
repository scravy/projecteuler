import Data.Char
import Data.Function
import Data.List

-- utilities
println  = putStrLn . show
readNums = map (map read . words) . lines
nodup    = map head . group . sort

-- math
divisibleBy d q = d `rem` q == 0
factorial n = product [ 1 .. n ]
choose n k = factorial n `quot` (factorial k * (factorial (n - k)))
isqrt n = head $ dropWhile (\x -> x * x > n)
               $ iterate (\x -> (x + n `quot` x) `quot` 2) (n `quot` 2)

-- sequences
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
primes = 2 : filter (\x -> all (not . (x `divisibleBy`)) (takeWhile (<= isqrt x) primes)) [ 3 .. ]
triangleNums = 1 : 3 : zipWith (+) [ 3 .. ] (tail triangleNums)

-- predicates
isPrime n = n == head (dropWhile (< n) primes)
isPalindrome n = show n == (reverse . show) n

primeFactors = nodup . factorize
factorize n = if null fs then [n] else fs
 where
  fs = fact n primes
  fact _ [] = []
  fact 1 _  = []
  fact x fs@(f : ft)
    | r == 0    = f : fact q fs
    | otherwise = fact x ft
   where (q, r) = quotRem x f

factorize2 = map (\xs -> (head xs, length xs)) . group . factorize

divisors 1 = [1]
divisors n = 1 : ds ++ (dropWhile (== sr) $ reverse $ (n : map (n `quot`) ds))
 where
  sr = isqrt n
  ds = filter (n `divisibleBy`) [ 2 .. sr ]

properDivisors = init . divisors

euler1 = sum [ x | x <- [ 1 .. 999 ], x `divisibleBy` 3 || x `divisibleBy` 5 ]

euler2 = sum $ filter even $ takeWhile (<= 4000000) fibs

euler3 = maximum $ primeFactors 600851475143

euler4 = maximum [ x * y | x <- [ 100 .. 999 ], y <- [ x .. 999 ],
                           isPalindrome (x * y) ]

combined = foldr (combine max) [] . map factorize2
 where
  combine op as@(a@(fa, na) : at) bs@(b@(fb, nb) : bt)
    | fa == fb  = (fa, op na nb) : combine op at bt
    | fa <  fb  = a : combine op at bs
    | otherwise = b : combine op as bt
  combine op as [] = as
  combine op [] bs = bs

euler5 = product $ map (\(f, n) -> f ^ n) (combined [ 2 .. 20 ])

euler6 = (^ 2) (sum [ 1 .. 100 ]) - sum (map (^ 2) [ 1 .. 100 ])

euler7 = primes !! 10000

euler8 = digits <$> readFile "euler8.txt" >>= println . maxProduct
 where
  maxProduct = maximum . map product . groups 13
  groups n   = filter ((== n) . length) . map (take n) . tails
  digits     = map ((\x -> x - ord '0') . ord) . filter isDigit

euler9 = head [ a * b * c | a <- [ 1 .. 999 ], b <- [ succ a .. 999 ],
                            c <- [ 1000 - (a + b) ], a ^ 2 + b ^ 2 == c ^ 2 ]

euler10 = sum $ takeWhile (< 2000000) primes

euler11 = readNums <$> readFile "euler11.txt" >>= println . f
 where
  f grid = maximum [ product l | x <- [ 0 .. 16 ], y <- [ 0 .. 16 ],
                                 l <- [ vert x y, hori x y, diag1 x y, diag2 x y ] ]
   where
    vert  x y   = line [ x .. x + 3 ] (repeat y)
    hori  x y   = line [ y .. y + 3 ] (repeat x)
    diag1 x y   = line [ x .. x + 3 ] [ y .. y + 3 ]
    diag2 x y   = line [ x .. x + 3 ] (reverse [ y .. y + 3 ])
    line  xs ys = map pick $ zip xs ys
    pick (x, y) = grid !! x !! y

euler12 = fst $ head $ dropWhile f $ zip triangleNums (map divisors triangleNums)
 where f (n, ds) = length ds <= 500

euler13 = do
  s <- (map read . lines) <$> readFile "euler13.txt"
  putStrLn (take 10 $ show (sum s))

euler15 = 40 `choose` 20

maximumPathSum fileName = do
  rows <- (map (map read . words) . lines) <$> readFile fileName
  putStrLn (show (maximum (foldl f [] rows)))
 where
  f as bs = map (uncurry max) (zip (zipWith (+) (0 : as) bs) (zipWith (+) (as ++ [0]) bs))

euler18 = maximumPathSum "euler18.txt"

euler25 = fst $ head $ dropWhile ((< 10 ^ 999) . snd) $ zip [ 0 .. ] fibs

euler27 = let (a, b) = fst (maximumBy (compare `on` snd) ns) in a * b
 where
  fs = [ ((a, b), \n -> n ^ 2 + a * n + b) | a <- [ -999 .. 999 ], b <- [ -1000 .. 1000 ] ]
  ns = map (\f -> (fst f, length $ takeWhile isPrime $ map (snd f) [ 0 .. ])) fs

euler67 = maximumPathSum "euler67.txt"

