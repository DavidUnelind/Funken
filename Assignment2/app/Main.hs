{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import Lib

main :: IO ()
main = someFunc

scoreMatch :: Int
scoreMatch = 0
scoreMismatch :: Int
scoreMismatch = -1
scoreSpace :: Int
scoreSpace = -1

string1 :: String
string1 = "writers"
string2 :: String
string2 = "vintner"

similarityScore :: String -> String -> Int
similarityScore s1 s2 = uncurry calcScore (head (optAlignments s1 s2))

similarityChecker :: String -> String -> Int -> Int
similarityChecker [] _ nbr = nbr
similarityChecker _ [] nbr = nbr
similarityChecker (x:s1) (y:s2) nbr 
    | x == y = similarityChecker s1 s2 (nbr + scoreMatch)
    | x == '-' || y == '-' = similarityChecker s1 s2 (nbr + scoreSpace)
    | x /= y = similarityChecker s1 s2 (nbr + scoreMismatch)
    | otherwise = nbr

calcScore :: String -> String -> Int
calcScore s1 s2 = similarityChecker s1 s2 0


-- b) the function attachHeads takes two arguments and a list of a pair containing two lists,
--    then returns a list of a pair containing two new lists with the elements h1 and h2 at the head of
--    each list. 
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter (\i -> valueFcn i == maximum (map valueFcn xs)) xs


type AlignmentType = (String, String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
optAlignments [] (x:xs) = attachHeads '-' x (optAlignments [] xs)
optAlignments (y:ys) [] = attachHeads y '-' (optAlignments ys [])
optAlignments (y:ys) (x:xs) = maximaBy (uncurry calcScore) (concat [case1, case2, case3])
    where
        case1 = attachHeads y x (optAlignments ys xs)
        case2 = attachHeads y '-' (optAlignments ys (x:xs))
        case3 = attachHeads '-' x  (optAlignments (y:ys) xs)

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments s1 s2 = do
    putStrLn ("There are " ++ show x ++ " optimal alignments:") 
    mapM_ (putStrLn . formatStrings) optList
    putStrLn ("There were " ++ show x ++ " optimal alignments!")
    where 
        optList = optAlignments s1 s2
        x = length optList
        formatStrings (a, b) = a ++ "\n" ++ b ++ "\n"