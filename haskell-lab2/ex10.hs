fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True -- dekompozycja
fst2Eq _                    = False


fstDiv2 :: Integral a => [a] -> Bool
fstDiv2 (x : y : _) | y `mod` x == 0 = True
fstDiv2 _ = False