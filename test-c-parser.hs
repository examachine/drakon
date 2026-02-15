-- Test for C parser

import Text.Parsec
import Text.Parsec.String

-- Example test function
parseCProgram :: String -> Either ParseError ()
parseCProgram input = parse cProgram "C Program" input

-- Placeholder for actual C program parsing implementation
cProgram :: Parser ()
cProgram = return ()

main :: IO ()
main = do
    let testInput = "int main() { return 0; }"
    case parseCProgram testInput of
        Left err -> print err
        Right _  -> putStrLn "Parse successful!"