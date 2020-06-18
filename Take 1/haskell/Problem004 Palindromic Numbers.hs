xs = [f a b | a <- [100..999], b <- [a..999]]
    where
        f a b = let p = a * b
                in  ((p, (a, b)), div p 100000 == mod p 10 &&
                     mod (div p 10000) 10 == mod (div p 10) 10 &&
                     mod (div p 1000) 10 == mod (div p 100) 10 )

main = putStrLn $ show $ maximum $ map (fst . fst) $ filter snd xs

