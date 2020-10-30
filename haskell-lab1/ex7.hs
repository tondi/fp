not' :: Bool -> Bool
not' True = False
not' False = True


or' :: (Bool, Bool) -> Bool
or' (True, True) = True
or' (True, False) = True
or' (False, True) = True
or' (False, False) = False


and' :: (Bool, Bool) -> Bool
and' (True, True) = True
and' (True, False) = False
and' (False, True) = False
and' (False, False) = False

nand' :: (Bool, Bool) -> Bool
nand' (True, True) = True
nand' (True, False) = True
nand' (False, True) = True
nand' (False, False) = False


xor' :: (Bool, Bool) -> Bool
xor' (True, True) = False
xor' (True, False) = True
xor' (False, True) = True
xor' (False, False) = False
