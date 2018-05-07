import Data.Array.Unboxed
import Data.Char
import Data.Function
import Data.List
import Data.Ratio

import Numeric

import System.Environment
import System.Process

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

tr a b = map (\x -> if x == a then b else x)

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

properDivisors :: Integral a => a -> [a]
properDivisors = init . divisors

coprimes = concatMap id (zipWith (\a b -> [a, b]) (cps (2, 1)) (cps (3, 1)))
 where
  cps (m, n) = (m, n) : map head bs ++ concatMap tail bs
   where
    bs = map cps [ (2 * m - n, m), (2 * m + n, m), (m + 2 * n, n) ]

totients = 0 : 1 : map totient [ 2 .. ]
 where
  totient n = case factorize2 n of
    [(p, 1)] -> pred p
    [(p, k)] -> p ^ k - p ^ (k - 1)
    factors -> product (map (phi . (\(p, k) -> p ^ k)) factors)

fak digit = case digit of
  '0' -> 1; '1' -> 1; '2' -> 2; '3' -> 6; '4' -> 24; '5' -> 120; '6' -> 720; '7' -> 5040; '8' -> 40320; '9' -> 362880

rotations' xs = [ drop n xs ++ take n xs | n <- [ 1 .. pred $ length xs ] ]

rotations :: (Show a, Read a) => a -> [a]
rotations = map read . rotations' . show

phi = (totients !!) . fromInteger

-- sequences
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
primes = 2 : filter (\x -> all (not . (x `divisibleBy`)) (takeWhile (<= isqrt x) primes)) [ 3 .. ]
triangleNums = 1 : 3 : zipWith (+) [ 3 .. ] (tail triangleNums)
abundantNumbers = [ x | x <- [2 .. ], let s = sum (properDivisors x) in s > x ]

-- predicates
isPrime n = n == head (dropWhile (< n) primes)
isPalindrome n = show n == (reverse . show) n
isPalindromeAtBase b n = let s = showIntAtBase b (head . show) n "" in s == reverse s

isPandigital xs = sort (xs >>= show) == "123456789"

maximumPathSum = maximum . foldl f [] . readNums
 where
  f as bs = map (uncurry max) (zip (zipWith (+) (0 : as) bs) (zipWith (+) (as ++ [0]) bs))

euler n | n `elem` javaProblems = runJava n

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

euler 8 = (maxProduct . digits) <$> readFile "euler8.txt"
 where
  maxProduct = maximum . map product . groups 13
  groups n   = filter ((== n) . length) . map (take n) . tails

euler 9 = return $ head [ a * b * c | a <- [ 1 .. 999 ], b <- [ succ a .. 999 ],
                                      c <- [ 1000 - (a + b) ], a ^ 2 + b ^ 2 == c ^ 2 ]

euler 10 = return $ sum $ takeWhile (< 2000000) primes

euler 11 = (f . readNums) <$> readFile "euler11.txt"
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

euler 12 = return $ fst $ head $ dropWhile f $ zip triangleNums (map divisors triangleNums)
 where f (n, ds) = length ds <= 500

euler 13 = (read . take 10 . show . sum . map read . lines) <$> readFile "euler13.txt"

euler 15 = return $ 40 `choose` 20

euler 16 = return $ sum (digits (show (2 ^ 1000)))

euler 17 = return $ toInteger $ length $ filter isLetter $ concatMap tell [ 1 .. 1000 ]

euler 18 = maximumPathSum <$> readFile "euler18.txt"

euler 19 = return $ 1200 `div` 7

euler 20 = return $ sum $ digits $ show $ factorial 100

euler 21 = return $ sum [ toInteger i | i <- [ 2 .. 9999 ], isAmicable i ]
 where
  isAmicable i = ((filter (/= i) (pick i)) >>= pick) == [i]
  pick i
    | ds ! i <= 9999 && i >= 2 = [ ds ! i ]
    | otherwise = []
  ds :: UArray Int Int
  ds = array (0, 9999) (zo ++ [ (i, sum $ properDivisors i ) | i <- [ 2 .. 9999 ] ])
  zo = [ (0, 0), (1, 0) ]

euler 22 = sum . zipWith (*) [ 1 .. ] . map alphabeticalValue . load <$> readFile "euler22.txt"
 where
  load = sort . map (filter isAlpha) . lines . tr ',' '\n'
  alphabeticalValue = sum . map (toInteger . succ . flip (-) (ord 'A') . ord)

euler 25 = return $ fst $ head $ dropWhile ((< 10 ^ 999) . snd) $ zip [ 0 .. ] fibs

euler 27 = return $ a * b
 where
  (a, b) = fst (maximumBy (compare `on` snd) ns)
  fs = [ ((a, b), \n -> n ^ 2 + a * n + b) | a <- [ -999 .. 999 ], b <- [ -1000 .. 1000 ] ]
  ns = map (\f -> (fst f, length $ takeWhile isPrime $ map (snd f) [ 0 .. ])) fs

euler 29 = return $ toInteger $ length $ group $ sort [ a ^ b | a <- [ 2 .. 100 ],
                                                                b <- [ 2 .. 100 ]]

euler 30 = return result
 where
  ds = [ 0 .. 9 ]
  p  = 5
  result = sum [ n | a <- ds, b <- ds, c <- ds, d <- ds, e <- ds, f <- ds,
                     n <- [ a + b * 10 + c * 100 + d * 1000 + e * 10000 + f * 100000 ],
                     n >= 10 && n == a ^ p + b ^ p + c ^ p + d ^ p + e ^ p + f ^ p ]

euler 31 = return $ toInteger $ length $ fs 200 [ 200, 100, 50, 20, 10, 5, 2, 1 ]
 where
  fs n cs = case cs of
    [] -> []
    [1] -> [[(n, 1)]]
    (x : xs) -> do
      z <- zip [0 .. (n `div` x)] (repeat x)
      map (z :) (fs (n - uncurry (*) z) xs)

euler 32 = return $ sum $ nub $ f 1 9 1234 9876 ++ f 12 98 123 987
 where
  f a b c d = [ z | x <- [ a .. b ], y <- [ c .. d ], z <- [ x * y ], isPandigital [x, y, z] ]

euler 33 = return $ denominator $ product [ n % d | x <- [1..9], y <- [1..9], z <- [1..9], n <- [x*10+y], d <- [y*10+z], n < d, n%d == x%z]

euler 34 = return $ sum [ x | x <- [10 .. 100000], (sum . map fak . show) x == x ]

euler 35 = return $ toInteger $ length $ filter (all (`elem` ps) . rotations) ps
 where
  ps = takeWhile (< 1000000) primes

euler 36 = return $ sum [ x | x <- [ 1 .. 999999 ], isPalindrome x && isPalindromeAtBase 2 x ]

euler 67 = maximumPathSum <$> readFile "euler67.txt"

{-
euler 69 = return $ fst $ maximumBy (compare `on` snd) $ reverse $ drop 2
                  $ zipWith zipper [ 0 .. 1000000 ] totients
 where
  zipper n b = (n, fromInteger n / fromInteger b)
-}

euler _ = return 0

javaProblems = [ 14, 23, 26, 28, 48 ]

runJava :: Int -> IO Integer
runJava n = read <$> readCreateProcess (shell $ "java -jar euler.jar " ++ show n) ""

main = do
  problems <- map read <$> getArgs
  (flip mapM) (if null problems then [ 1 .. 700 ] else problems) $ \n -> do
    euler n >>= \r -> case r of
      0 -> return ()
      _ -> putStr (show n) >> putStr "\t" >> putStrLn (show r)

