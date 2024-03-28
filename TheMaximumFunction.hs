maxi :: Ord a => a -> a -> a
maxi x y
    | x < y = y
    | otherwise = x