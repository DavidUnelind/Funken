module Main where

import Parser

main :: IO ()
main = do
    putStrLn "Enter a string to parse:"
    input <- getLine
    let result = parse word "" input
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right parsed -> putStrLn $ "Parsed word: " ++ parsed
