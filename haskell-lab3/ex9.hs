sumWith g []     = 0
sumWith g (x:xs) = g x + sumWith g xs -- (+) (g x) (sumWith g xs)

prodWith g []     = 1
prodWith g (x:xs) = g x * prodWith g xs -- (*) (g x) (prodWith g xs)



sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x + acc) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x * acc) g xs



foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z = go z
    where go acc [] = acc
          go acc (x:xs) = go (f x acc) xs

sumWith'' g  = foldr' (\x acc -> g x + acc) 0
prodWith'' g = foldr' (\x acc -> g x * acc) 1


foldl' f z [] = z
foldl' f z (x:xs) = foldl f (f z x) xs


sumWith''' g  = foldl' (\acc x -> g x + acc) 0
prodWith''' g = foldl' (\acc x -> g x * acc) 1

-- foldr' (+) 0 [1..10^6]
-- (0.60 secs, 258,166,944 bytes)

-- foldr (+) 0 [1..10^6] -- foldr z biblioteki standardowej
-- (0.21 secs, 161,642,504 bytes)