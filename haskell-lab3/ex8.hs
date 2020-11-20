import Data.Char

-- doubleElems []     = []
-- doubleElems (x:xs) = 2 * x : doubleElems xs

-- sqrElems [] = []
-- sqrElems (x:xs) = x^2 : sqrElems xs

-- lowerCase [] = []
-- lowerCase (x:xs) = toLower x : lowerCase xs

-- sqrElems [1..3] -- [1,4,9]
-- lowerCase "ABCD" -- "abcd", konieczny import Data.Char

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs
    

doubleElems = map' (*2)
sqrElems = map' (^2)
lowerCase = map' toLower

mapComp p xs = [ p x | x <- xs]

doubleElemsComp' :: [Integer] -> [Integer]
doubleElemsComp' = mapComp (*2)

sqrElemsComp' = mapComp (^2)

-- length . filter even $ doubleElems [1..10^7]
-- (4.48 secs, 4,240,122,912 bytes)

-- length . filter even . map (*2) $ [1..10^7] 
-- (1.77 secs, 3,360,122,800 bytes)