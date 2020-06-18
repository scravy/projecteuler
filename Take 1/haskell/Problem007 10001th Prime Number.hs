primes = let sieve (p : xs) = p : sieve [x | x <- xs, mod x p > 0] in sieve [2..]

main = putStrLn $ show $ head $ drop 10000 primes -- 104743

