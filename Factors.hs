smallestFactor :: (Integral a, Ord a) => a -> a
smallestFactor n
    | n > 1 = nextFactor n (n - 1)
    | otherwise = 1

nextFactor :: (Integral a, Ord a) => a -> a -> a
nextFactor n k
    | mod n k == 0 = div n k
    | otherwise = nextFactor n (k - 1)