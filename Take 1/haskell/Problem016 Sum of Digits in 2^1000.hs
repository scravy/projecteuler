two1000 = 2^1000

sumDigits :: Integer -> Integer
sumDigits n = f 0 n
    where   f s 0 = s
            f s n = f (s + mod n 10) (div n 10)

main = putStrLn $ show $ sumDigits two1000

