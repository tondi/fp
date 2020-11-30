-- product type example (one constructor)
-- data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

-- xCoord :: CartInt2DVec -> Int
-- xCoord (MkCartInt2DVec x _) = x

-- yCoord :: CartInt2DVec -> Int
-- yCoord (MkCartInt2DVec _ y) = y

-- Co dałoby wprowadzenie w deklaracji CartInt2DVec aliasów/synonimów
-- odpowiedź: czytelność

data Cart2DVec' a = MkCart2DVec' a a

-- xCoord' :: Cart2DVec' a -> a
-- xCoord' (MkCart2DVec' x _) = x

-- yCoord' :: Cart2DVec' a -> a
-- yCoord' (MkCart2DVec' _ y) = y

-- data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

-- xCoord'' :: Cart2DVec'' a -> a
-- xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

-- yCoord'' :: Cart2DVec'' a -> a
-- yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal -- uwaga na kolejność x,y

-- ghci> :t x -- dlaczego ta funkcja istnieje (skoro jej nie deklarowaliśmy/definiowaliśmy)?
-- odpowiedź: jest zdefiniowana w {x = xVal, y = _}
-- ghci> :t y -- jw.?
-- odpowiedź: jest zdefiniowana w {y = yVal, x = _}



-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x


-- enum type example (special case of sum type)
data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

-- data Cart3DVec a = Cart3DVec a a a

data Cart3DVec = MkCart3DVec Int Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: Cart3DVec -> Int
xCoord (MkCart3DVec x _ _) = x

yCoord :: Cart3DVec -> Int
yCoord (MkCart3DVec _ y _) = y

zCoord :: Cart3DVec -> Int
zCoord (MkCart3DVec _ _ z) = z

-- *Main> xCoord $ MkCart3DVec 1 2 3
-- 1
-- *Main> yCoord $ MkCart3DVec 1 2 3
-- 2
-- *Main> zCoord $ MkCart3DVec 1 2 3
-- 3

data Cart3DVec' a = MkCart3DVec' {x::a, y::a, z::a} -- konwencja: prefix 'Mk' dla konstruktora

-- data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

-- *Main> x $ MkCart3DVec' 1 2 3
-- 1
-- *Main> y $ MkCart3DVec' 1 2 3
-- 2
-- *Main> z $ MkCart3DVec' 1 2 3
-- 3

data Shape = Circle Float |
             Rectangle Float Float deriving Show
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b


data Tree a = EmptyT |
              Node a (Tree a) (Tree a)
              deriving Show

rootValue :: Tree a -> a
rootValue (EmptyT) = error "Cannot get root from EmptyT"
rootValue (Node x _ _) = x

-- *Main> rootValue $ Node 2 (Node 3 (EmptyT)(EmptyT)) (EmptyT)
-- 2


data TrafficLights = ColorRed |
                     ColorYellow |
                     ColorGreen
actionFor :: TrafficLights -> String
actionFor ColorRed = "Stop"
actionFor ColorYellow = "Slow down, wait for next light"
actionFor ColorGreen = "Drive"
    


