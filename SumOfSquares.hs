sumsq :: (Num a, Enum a) => a -> a
sumsq n = sum (map (^2) [1..n])