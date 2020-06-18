import Data.List

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

problem2 upto = foldl' (+) 0 $ filter even $ takeWhile (< upto) fibs

main = putStrLn $ show $ problem2 4000000
