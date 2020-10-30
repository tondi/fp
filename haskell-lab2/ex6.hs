fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)

-- zlozonosc: wykladnicza O(2^n)


sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs


prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' [] = 0
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int -- length' [1,1,1,1] = 4
length' [] = 0
length' (x) = 1 + length' (init x)

or' :: [Bool] -> Bool -- or' [True, False, True] = True
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = False
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True
elem' _ [] = False
elem' y (x:xs) = x == y || elem y xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) = [x * 2] ++ doubleAll xs

squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x:xs) = [x ^ 2] ++ squareAll xs

-- Czy można wskazać w implementacjach powtarzające się schematy?
-- tak: dzielenie listy na element i reszte, operacja dwuargumentowa na elemencie i funkcji z reszta

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

-- dodany zostal acc, ktory przechowuje wartosc

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

-- usunieta zostala deklaracja xs

