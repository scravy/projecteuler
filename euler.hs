import Data.Char
import Data.Function
import Data.List

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isqrt n = head $ dropWhile (\x -> x * x > n) $ iterate (\x -> (x + n `quot` x) `quot` 2) (n `quot` 2)

primes = 2 : filter (\x -> all (\p -> x `rem` p /= 0) (takeWhile (<= isqrt x) primes)) [ 3 .. ]

isPrime n = n == head (dropWhile (< n) primes)

isPalindrome n = show n == (reverse . show) n

factors n = filter (\p -> n `rem` p == 0) (takeWhile (<= n `quot` 2) primes)

factorize n = if null fs then [n] else fs
 where
  fs = h n (takeWhile (<= n `quot` 2) primes)
  h _ [] = []
  h x fs@(f : ft)
    | r == 0    = f : h q fs
    | otherwise = h x ft
   where (q, r) = quotRem x f

factorize2 = map (\xs -> (head xs, length xs)) . group . factorize

euler1 = sum [ x | x <- [ 1 .. 999 ], x `rem` 3 == 0 || x `rem` 5 == 0 ]

euler2 = sum $ filter even $ takeWhile (<= 4000000) fibs

euler3 = maximum $ factors 600851475143

euler4 = maximum [ x * y | x <- [ 100 .. 999 ], y <- [ x .. 999 ], isPalindrome (x * y) ]

combined = foldr (combine max) [] . map factorize2
 where
  combine op as@(a@(fa, na) : at) bs@(b@(fb, nb) : bt)
    | fa == fb  = (fa, op na nb) : combine op at bt
    | fa <  fb  = a : combine op at bs
    | otherwise = b : combine op as bt
  combine op as [] = as
  combine op [] bs = bs

euler5 = product $ map (\(f, n) -> f ^ n) (combined [ 2 .. 20 ])

euler6 = (^ 2) (sum [1 .. 100]) - sum (map (^ 2) [1 .. 100])

euler7 = primes !! 10000

euler8 = maximum (map product (groups 13))
 where
  groups n = filter ((== n) . length) $ map (take n) $ tails digits
  digits = map ((\x -> x - ord '0') . ord) n
  n = "73167176531330624919225119674426574742355349194934\
      \96983520312774506326239578318016984801869478851843\ 
      \85861560789112949495459501737958331952853208805511\
      \12540698747158523863050715693290963295227443043557\
      \66896648950445244523161731856403098711121722383113\
      \62229893423380308135336276614282806444486645238749\
      \30358907296290491560440772390713810515859307960866\
      \70172427121883998797908792274921901699720888093776\
      \65727333001053367881220235421809751254540594752243\
      \52584907711670556013604839586446706324415722155397\
      \53697817977846174064955149290862569321978468622482\
      \83972241375657056057490261407972968652414535100474\
      \82166370484403199890008895243450658541227588666881\
      \16427171479924442928230863465674813919123162824586\
      \17866458359124566529476545682848912883142607690042\
      \24219022671055626321111109370544217506941658960408\
      \07198403850962455444362981230987879927244284909188\
      \84580156166097919133875499200524063689912560717606\
      \05886116467109405077541002256983155200055935729725\
      \71636269561882670428252483600823257530420752963450"

euler9 = head [ a * b * c | a <- [ 1 .. 999 ], b <- [ succ a .. 999 ], c <- [ 1000 - (a + b) ], a ^ 2 + b ^ 2 == c ^ 2 ]

euler10 = sum $ takeWhile (< 2000000) primes

euler11 = maximum [ product l | x <- [ 0 .. 16 ], y <- [ 0 .. 16 ], l <- [ vert x y, hori x y, diag1 x y, diag2 x y ] ]
 where
  vert x y = line [ x .. x + 3 ] (repeat y)
  hori x y = line [ y .. y + 3 ] (repeat x)
  diag1 x y = line [ x .. x + 3 ] [ y .. y + 3 ]
  diag2 x y = line [ x .. x + 3 ] (reverse [ y .. y + 3 ])
  line xs ys = map pick $ zip xs ys
  pick (x, y) = grid !! x !! y
  grid = (map (map read . words) . lines)
    "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08\n\
    \49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00\n\
    \81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65\n\
    \52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91\n\
    \22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80\n\
    \24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50\n\
    \32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70\n\
    \67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21\n\
    \24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72\n\
    \21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95\n\
    \78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92\n\
    \16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57\n\
    \86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58\n\
    \19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40\n\
    \04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66\n\
    \88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69\n\
    \04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36\n\
    \20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16\n\
    \20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54\n\
    \01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"

divisors 1 = [1]
divisors n = 1 : ds ++ (dropWhile (== sr) $ reverse $ (n : map (n `quot`) ds))
 where
  sr = isqrt n
  ds = filter (\x -> n `rem` x == 0) [ 2 .. sr ]

triangleNumbers = 1 : 3 : zipWith (+) [ 3 .. ] (tail triangleNumbers)

euler12 = fst $ head $ dropWhile f $ zip triangleNumbers (map divisors triangleNumbers)
 where f (n, ds) = length ds <= 500

euler13 = do
  s <- (map read . lines) <$> readFile "euler13.txt"
  putStrLn (take 10 $ show (sum s))

factorial n = product [ 1 .. n ]

choose n k = factorial n `quot` (factorial k * (factorial (n - k)))

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

