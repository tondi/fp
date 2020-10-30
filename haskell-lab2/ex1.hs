myFun x = 2 * x

add2T :: Num a => (a, a) -> a
add2T (x,y) = x + y

add2C :: Num a => a -> (a -> a)
add2C x y = x + y

-- tutaj: prawostronnie

-- bo:
-- f :: a -> b -> c -> d = a -> (b -> (c -> d)) -- prawostronna łączność
-- f 1 2.2 'c' = ((f 1) 2.2) 'c' -- lewostronna łączność

add3T :: Num a => (a, a, a) -> a
add3T (x,y,z) = x + y + z

add3C :: Num a => a -> (a -> (a -> a))
add3C x y z = x + y + z

-- ((add3C 1) 2) 3

curry2 :: ((a,b) -> c) -> a -> b -> c
curry2 f x y = f(x, y)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f(x, y) = f x y

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

