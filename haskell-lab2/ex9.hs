qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (leftPart xs) ++ [x] ++ qsort (rightPart xs)
    where 
        leftPart xs = filter (<= x) xs
        rightPart xs = filter (> x) xs
