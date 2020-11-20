sqr x = x^2

funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

fact :: (Eq x, Num x) => x -> x
fact 0 = 1;
fact x = x * fact (x - 1)

expApproxUpTo :: (Fractional t, Integral t) => t -> t -> t
expApproxUpTo n = 
    \pow -> loop 0 [0..n]
    where { loop acc [] = acc; loop acc (a:all) = loop ((pow ^ a / fact a) + acc) all }
