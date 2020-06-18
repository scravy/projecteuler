

pyt = [a * b * (1000-a-b) | a<-[1..1000], b<-[a+1..1000-a], a^2 + b^2 == (1000-a-b)^2 ]

main = putStrLn $ show $ pyt
