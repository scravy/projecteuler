import Data.List

problem1 upto = a + b - c
    where   a = foldl' (+) 0 [ 0,3 .. pred upto ]
            b = foldl' (+) 0 [ 0,5 .. pred upto ]
            c = foldl' (+) 0 [ 0,15.. pred upto ]

main = putStrLn $ show $ problem1 1000
