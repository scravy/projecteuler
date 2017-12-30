import Data.Char
import Data.Function
import Data.List

import System.Environment

-- utilities
readNums = map (map read . words) . lines
nodup    = map head . group . sort
digits   = map (toInteger . (\x -> x - ord '0') . ord) . filter isDigit
tell n   = unwords $ (filter (not . null)) $ case n of
  0                     -> [ "zero" ]
  1000                  -> [ "one", "thousand" ]
  _ | n > 10 && n < 20  -> [ w2 !! r ]
  _ | n < 100           -> [ w10 !! q, w1 !! r ]
  _ | p == 0            -> [ w1 !! o, "hundred" ]
  _ | q == 1            -> [ w1 !! o, "hundred", "and", w2 !! r ]
  _                     -> [ w1 !! o, "hundred", "and", w10 !! q, w1 !! r ]
 where
  (o, p) = n `quotRem` 100
  (q, r) = p `quotRem` 10
  w1  = "" : words "one two three four five six seven eight nine"
  w2  = words "ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen"
  w10 = "" : words "ten twenty thirty forty fifty sixty seventy eighty ninety"

-- math
divisibleBy d q = d `rem` q == 0
factorial n = product [ 1 .. n ]
choose n k = factorial n `quot` (factorial k * (factorial (n - k)))
isqrt n = head $ dropWhile (\x -> x * x > n)
               $ iterate (\x -> (x + n `quot` x) `quot` 2) (n `quot` 2)

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

-- sequences
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
primes = 2 : filter (\x -> all (not . (x `divisibleBy`)) (takeWhile (<= isqrt x) primes)) [ 3 .. ]
triangleNums = 1 : 3 : zipWith (+) [ 3 .. ] (tail triangleNums)

-- predicates
isPrime n = n == head (dropWhile (< n) primes)
isPalindrome n = show n == (reverse . show) n

maximumPathSum = maximum . foldl f [] . map (map read . words) . lines
 where
  f as bs = map (uncurry max) (zip (zipWith (+) (0 : as) bs) (zipWith (+) (as ++ [0]) bs))

euler 1 = return $ sum [ x | x <- [ 1 .. 999 ], x `divisibleBy` 3 || x `divisibleBy` 5 ]

euler 2 = return $ sum $ filter even $ takeWhile (<= 4000000) fibs

euler 3 = return $ maximum $ primeFactors 600851475143

euler 4 = return $ maximum [ x * y | x <- [ 100 .. 999 ], y <- [ x .. 999 ],
                                     isPalindrome (x * y) ]

euler 5 = return $ product $ map (\(f, n) -> f ^ n) (combined [ 2 .. 20 ])
 where
  combined = foldr (combine max) [] . map factorize2
   where
    combine op as@(a@(fa, na) : at) bs@(b@(fb, nb) : bt)
      | fa == fb  = (fa, op na nb) : combine op at bt
      | fa <  fb  = a : combine op at bs
      | otherwise = b : combine op as bt
    combine op as [] = as
    combine op [] bs = bs

euler 6 = return $ (^ 2) (sum [ 1 .. 100 ]) - sum (map (^ 2) [ 1 .. 100 ])

euler 7 = return $ primes !! 10000

euler 8 = digits <$> readFile "euler8.txt" >>= return . maxProduct
 where
  maxProduct = maximum . map product . groups 13
  groups n   = filter ((== n) . length) . map (take n) . tails

euler 9 = return $ head [ a * b * c | a <- [ 1 .. 999 ], b <- [ succ a .. 999 ],
                                      c <- [ 1000 - (a + b) ], a ^ 2 + b ^ 2 == c ^ 2 ]

euler 10 = return $ sum $ takeWhile (< 2000000) primes

euler 11 = readNums <$> readFile "euler11.txt" >>= return . f
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

euler 12 = return $fst $ head $ dropWhile f $ zip triangleNums (map divisors triangleNums)
 where f (n, ds) = length ds <= 500

euler 13 = (map read . lines) <$> readFile "euler13.txt" >>= return . read . take 10 . show . sum

euler 15 = return $ 40 `choose` 20

euler 16 = return $ sum (digits (show (2 ^ 1000)))

euler 17 = return $ toInteger $ length $ filter isLetter $ concatMap tell [ 1 .. 1000 ]

euler 18 = maximumPathSum <$> readFile "euler18.txt"

euler 25 = return $ fst $ head $ dropWhile ((< 10 ^ 999) . snd) $ zip [ 0 .. ] fibs

euler 27 = return $ a * b
 where
  (a, b) = fst (maximumBy (compare `on` snd) ns)
  fs = [ ((a, b), \n -> n ^ 2 + a * n + b) | a <- [ -999 .. 999 ], b <- [ -1000 .. 1000 ] ]
  ns = map (\f -> (fst f, length $ takeWhile isPrime $ map (snd f) [ 0 .. ])) fs

euler 67 = maximumPathSum <$> readFile "euler67.txt"

euler _  = return 0

main = do
  problems <- map read <$> getArgs
  (flip mapM) (if null problems then [ 1 .. 700 ] else problems) $ \n -> do
    euler n >>= \r -> case r of
      0 -> return ()
      _ -> putStr (show n) >> putStr "\t" >> putStrLn (show r)
  
