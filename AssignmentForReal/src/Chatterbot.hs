module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.List.Split
import Data.Maybe

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
  r <- randomIO :: IO Float
  return (rulesApply ((map . map2) (id, pick r) brain)) --Samuel??? Help


rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply transformations p = try (transformationsApply "*" reflect transformations) p

reflect :: Phrase -> Phrase
reflect = map (\i -> fromJust (orElse (lookup i reflections) (Just i)))

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile content = map (\i -> (words (fst i), map (\j -> words (j)) (snd i))) content


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply pair p 
  | isJust (transformationsApply "*" id pair p) = reductionsApply pair (fromJust (transformationsApply "*" id pair p))
  | otherwise = p


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wc (x:xs) value
    | x == wc = value ++ substitute wc xs value
    | otherwise     = x : substitute wc xs value   


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wc xs list
    | not (elem wc xs) && xs == list = Just []
    | not (elem wc xs) && xs /= list = Nothing
    | wc == head xs = orElse (singleWildcardMatch xs list) (longerWildcardMatch xs list)
    | wc /= head xs && head xs == head list = match wc (tail xs) (tail list)
    | otherwise = Nothing


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs)
    | isJust (match wc ps xs) = Just [x] -- varför?????????
    | otherwise = Nothing

longerWildcardMatch _ (x:[]) = Nothing
longerWildcardMatch (wc:[]) xs = Just xs
longerWildcardMatch ps xs = mmap ((head xs):) (match (head ps) (ps) (tail xs)) -- varför?? Samuel hjälp


-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f input (original, translated)
  | isJust (match wc original input) = mmap (substitute wc translated . f) (match wc original input)
  | otherwise = Nothing

-- = function (substitute wc translated (match wc original input))


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f ((original, translated):xs) input
  | isJust (match wc original input) = mmap (substitute wc translated . f) (match wc original input)
  | otherwise = transformationsApply wc f xs input


