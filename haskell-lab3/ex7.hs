import Data.Char

-- onlyEven [] = []
-- onlyEven (x:xs)
--  | x `mod` 2 == 0 = x : onlyEven xs
--  | otherwise      = onlyEven xs

-- onlyOdd [] = []
-- onlyOdd (x:xs)
--  | x `mod` 2 == 1 = x : onlyOdd xs
--  | otherwise      = onlyOdd xs

-- onlyUpper [] = []
-- onlyUpper (x:xs)
--  | isUpper x      = x : onlyUpper xs
--  | otherwise      = onlyUpper xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
    | p x        = x : filter' p xs
    | otherwise  = filter' p xs

onlyEven  = filter' $ \x -> x `mod` 2 == 0 
onlyOdd   = filter' $ \x -> x `mod` 2 == 1
onlyUpper = filter' $ \x -> isUpper x


-- length . onlyEven $ [1..10^6]
-- length . filter even $ [1..10^6]
-- (0.10 secs, 204,120,904 bytes)

-- length([x | x <- [1..10^6], x `mod` 2 == 0])
-- (0.41 secs, 196,120,872 bytes)