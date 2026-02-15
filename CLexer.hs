-- C Lexer based on C++ Lexer

module CLexer where

import Data.Char

-- Define the token types
data Token = Keyword String | Identifier String | Constant String | Operator String | Delimiter String deriving (Show, Eq)

-- Lexer function
lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | isSpace x = lexer xs
    | isAlpha x = let (word, rest) = span isAlphaNum (x:xs) in
                    if word `elem` keywords
                    then Keyword word : lexer rest
                    else Identifier word : lexer rest
    | isDigit x = let (num, rest) = span isDigit (x:xs) in Constant num : lexer rest
    | x `elem` operators = Operator [x] : lexer xs
    | x `elem` delimiters = Delimiter [x] : lexer xs
    | otherwise = error $ "Unknown character: " ++ [x]

-- Sample keywords, operators, and delimiters
keywords = ["int", "return", "void", "if", "else"]
operators = "+-*/="
delimiters = "()[]{};"

-- Add more functionality as needed