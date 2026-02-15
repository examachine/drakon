-- CParser.hs

-- This module defines a simple parser for C language constructs.

module CParser where

import Text.Parsec
import Text.Parsec.String (Parser)

-- A parser for identifiers (variable names)
identifier :: Parser String
identifier = do
    first <- letter
    rest <- many alphaNum
    return (first:rest)

-- A parser for integer literals
integer :: Parser Integer
integer = read <$> many1 digit

-- A parser for C statements (simple example)
statement :: Parser String
statement = do
    id <- identifier
    char '='
    num <- integer
    return (id ++ " = " ++ show num)

-- A parser for a block of statements
theBlock :: Parser [String]
theBlock = many statement

-- Example usage of the parser
-- parse theBlock "x = 10; y = 20; z = 30;"

