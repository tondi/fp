sgn :: Int -> Int
sgn n | n < 0 = -n
    | otherwise = -n


min3Int :: (Int, Int, Int) -> Int -- min (1,2,3)=1, min (1,1,3)=1
min3Int (a, b, c) = min a (min b c)