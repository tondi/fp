isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = all id . map (\(x,y) -> x <= y) . zip xs $ tail xs

everySecond :: [t] -> [t]
everySecond xs = map fst $ filter (odd . snd) $ zip xs [1..]