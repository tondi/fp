
prod'2 :: Num a => [a] -> a
prod'2 xs = if length xs == 0 then 0 
    else loop 1 xs
        where   loop acc [] = acc
                loop acc (x:xs) = loop (x * acc) xs


length'2 :: Num a => [a] -> a
length'2 = loop 0
    where loop acc [] = acc
          loop acc (x:xs) = loop (acc + 1) xs

