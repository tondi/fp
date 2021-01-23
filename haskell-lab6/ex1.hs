(<$<) :: (a -> b) -> a -> b
(<$<) = ($)

(>$>) :: a -> (a -> b) -> b
x >$> f = f x
infixl 0 >$>

(<.<) :: (b -> c) -> (a -> b) -> (a -> c)
(<.<) = (.)

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
f >.> g = g . f
infixl 9 >.>


safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

-- Dlaczego nie da się wykonać złożenia safeTail >.> safeTail?
-- odpowiedź: argument safeTail oczekuje typu [a], po złozeniu moze otrzymac Nothing, co spowodowało by błąd 

extractMaybe :: Maybe a -> a
extractMaybe Nothing  = error "Nothing inside!"
extractMaybe (Just x) = x


insertMaybe :: a -> Maybe a
insertMaybe = Just

-- -- (>^$>) = extract (^) and apply ($)
-- (>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- ma >^$> f = (extractMaybe ma) >$> f
-- infixl 1 >^$>

-- Dlaczego błąd pojawia się tylko w ostatnim wyrażeniu?
-- odpowiedz: na kadym kroku extractowana jest wartość, kiedy na wejściu extractMaybe pozostaje Nothing - zwracany jest błąd

(>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  >^$> _ = Nothing
(Just x) >^$> f = f x
infixl 1 >^$>

-- Dlaczego po modyfikacji (>^$>) powyższy błąd nie pojawia się?
-- odpowiedź: pozbyliśmy się 
-- ma >^$> f = (extractMaybe ma) >$> f
-- na rzecz lini
-- Nothing  >^$> _ = Nothing
-- która dla wartości Nothing zwraca Nothing zamiast rzucać błędem


f1 :: (Ord a, Num a) => a -> Maybe a
f1 x = if x > 0 then Just (x + 1) else Nothing

f2 :: (Eq a, Num a) => a -> Maybe a
f2 x = if x /= 0 then Just (10 * x) else Nothing

-- Kleisli composition
(>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
-- f >.>> g = \x -> g (extractMaybe (f x))

f >.>> g = \x -> f x >^$> g

-- extractIO
-- extractIO :: Maybe a -> a
-- extractIO Nothing  = error "Nothing inside!"
-- extractIO (Just x) = x

-- odpowiednik insertMaybe

-- odpowiednik (>^$>)
-- Monad ((->) r)
-- (>>=) :: m a -> (a -> m b) -> m b

-- czy istnieje odpowiednik extractMaybe?
-- nie, nie znalazłem

-- odpowiednik >.>>
-- Kleisli composition
-- (>>=) :: m a -> (a -> m b) -> m b


-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- vs
-- (>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
-- podobne, >.>> bardziej abstrakcyjne