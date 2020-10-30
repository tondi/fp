-- length [(a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100], a ^ 2 + b ^ 2 == c ^ 2]
-- odp: 52

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..(floor . sqrt . fromIntegral) n-1], n `mod` i == 0] == []
