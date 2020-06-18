

triangulars = [ div (x*(x+1)) 2 | x<-[1..] ]

divisors' n = [ d | d <- [2..truncate (sqrt (fromIntegral n))], mod n d == 0 ]

divisors n = 1 : d ++ reverse (n : [div n x | x <- d])
    where   d = divisors' n

numDivisors n = 2 + 2 * length (divisors' n)

triangularDivisors = map (\x -> (x, numDivisors x)) triangulars

main = putStrLn $ show $ head $ dropWhile ((<= 500) . snd) $ triangularDivisors

