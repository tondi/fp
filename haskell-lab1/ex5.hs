sgn :: Int -> Int
sgn n = if n < 0
       then -1
       else if n == 0
            then 0
            else 1


absInt :: Int -> Int -- absInt 2 = absInt (-2) = 2
absInt n = if n < 0
    then -n
    else n 


min2Int :: (Int, Int) -> Int -- min (1,2) = 1, min (-1, -1) = -1
min2Int (x, y) = if x < y
    then x
    else y