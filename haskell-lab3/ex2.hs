sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' []     = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum list = sumWith (\x -> x) list
sumSqr list = sumWith (\x -> x^2) list
sumCube list = sumWith (\x -> x^3) list
sumAbs list = sumWith (\x -> abs x) list

-- sumWith (\x -> x^5) [1..15]
listLength list = sumWith (\_ -> 1) list