

primes = let sieve (p : xs) = p : sieve [x | x <- xs, mod x p > 0] in sieve [2..]

factors num
    | null fs = []
    | True    = f : factors (div num f)
        where   fs = [x | x <- takeWhile (<= num) primes, mod num x == 0]
                f  = head fs

main = putStrLn $ show $ maximum $ factors 600851475143
