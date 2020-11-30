

import Main (flattenBTInorder)
newtype MyInt = MkMyInt Int

instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

-- ghci> MkMyInt 1 /= MkMyInt 2 -- dlaczego nie pojawia się błąd?
-- odpowiedź: kompilator automatycznie dodał definicję /= jako negację ==

instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

-- ghci> MkMyInt 1 < MkMyInt 2 -- dlaczego nie pojawia się błąd?
-- odpowiedź: < dodane automatycznie przez kompilator


instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromIntegral int)

instance Show MyInt where
  show (MkMyInt i) = "MkMyInt " ++ show i

--  dla newtype kontruktor jest usuwany w runtime, zostaje typ ktorym definiowalismy newtype

flattenBTInorder :: BinTree a -> [a]  -- napisać trzy wersje: Inorder, Inorder, postorder
flattenBTInorder EmptyBT = []
flattenBTInorder (NodeBT n lt rt) = flattenBTInorder lt ++ [n] ++ flattenBTInorder rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a) deriving Show

instance Eq a => Eq (BinTree a) where
  (==) (NodeBT a left right) (NodeBT b bleft bright) = flattenBTInorder a == flattenBTInorder b
