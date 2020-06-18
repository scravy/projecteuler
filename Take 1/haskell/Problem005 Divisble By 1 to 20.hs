


step = 2*3*5*7*11*13*17*19

mod20 n = zipWith mod (cycle [n]) [2..20]

check = all (== 0) . mod20

main = putStrLn $ show $ head $ filter check [step, 2*step ..]

