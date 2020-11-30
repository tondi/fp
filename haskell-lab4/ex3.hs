-- import Data.List

data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a) deriving Show
sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt


data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a) |
              Subs (Expr a) (Expr a) |
              Mul (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Subs e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Subs e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Mul e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego 
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + depthOfBT lt

flattenBTInorder :: BinTree a -> [a]  -- napisać trzy wersje: Inorder, Inorder, postorder
flattenBTInorder EmptyBT = []
flattenBTInorder (NodeBT n lt rt) = flattenBTInorder lt ++ [n] ++ flattenBTInorder rt

-- *Main> flattenBTInorder (NodeBT 1 (NodeBT 2 EmptyBT EmptyBT) (NodeBT 3 EmptyBT EmptyBT))
-- [2,1,3]

flattenBTPreorder :: BinTree a -> [a]  -- napisać trzy wersje: Preorder, Preorder, postorder
flattenBTPreorder EmptyBT = []
flattenBTPreorder (NodeBT n lt rt) = [n] ++ flattenBTPreorder lt ++ flattenBTPreorder rt

-- *Main> flattenBTPreorder (NodeBT 1 (NodeBT 2 EmptyBT EmptyBT) (NodeBT 3 EmptyBT EmptyBT))
-- [1,2,3]

flattenBTPostorder :: BinTree a -> [a]  -- napisać trzy wersje: Preorder, Preorder, postorder
flattenBTPostorder EmptyBT = []
flattenBTPostorder (NodeBT n lt rt) = flattenBTPostorder lt ++ flattenBTPostorder rt ++ [n]

-- *Main> flattenBTPostorder (NodeBT 1 (NodeBT 2 EmptyBT EmptyBT) (NodeBT 3 EmptyBT EmptyBT))
-- [2,3,1]

mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT _ EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT (f $ n) (go lt) (go rt)
    where go = mapBT f

insert :: Ord a => a -> BinTree a -> BinTree a -- ??
insert a EmptyBT = NodeBT a EmptyBT EmptyBT
insert a (NodeBT n lt rt)
    | a == n = NodeBT a lt rt
    | a < n = NodeBT n (insert a lt) rt
    | a > n = NodeBT n lt (insert a rt)

list2BST :: Ord a => [a] -> BinTree a -- list to Binary Search Tree (BST)
list2BST l = foldl (\tr v-> insert v tr) EmptyBT l