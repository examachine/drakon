-- C++ Lexer

module CPlusPlusLexer
where
import Parsec
import qualified ParsecToken as P
import ParsecLanguage( javaStyle )
import CPlusPlus

-- The language definition for C++

lexer = P.makeTokenParser cppDef

opNames = [ "+", "-", "*", "/", "%", "^", "&", "|", "~", "!", "=", "<", ">",
	    "+=", "-=", "*=", "/=", "%=", "^=", "&=", "|=", "<<", ">>",
	    ">>=", "<<=", "==", "!=", "<=", ">=", "&&", "||", "++", "--",
	    ",", "->*", "->"]

cppDef = javaStyle
	 { P.reservedNames  = ["class", "struct", "union"
			      , "operator"
			      , "case", "default"
                              , "private", "public", "protected"
			      , "friend", "typedef", "auto", "register"
			      , "static", "extern", "inline", "virtual"
                              , "new", "delete", "sizeof", "asm"
                              , "enum", "const", "volatile"
			      , "char", "short", "int", "long", "signed"
			      , "unsigned", "float", "double", "void"
			      , "break", "continue", "do", "else", "for"
			      , "goto", "if", "return", "switch", "while"
                            ]
	 , P.reservedOpNames = opNames
	 , P.caseSensitive  = True
	 }


identifier      = P.identifier lexer    
reserved        = P.reserved lexer    
operator        = P.operator lexer    
reservedOp      = P.reservedOp lexer

charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer
natural         = P.natural lexer 
integer         = P.integer lexer   
float           = P.float lexer
naturalOrFloat  = P.naturalOrFloat lexer 
decimal         = P.decimal lexer
hexadecimal     = P.hexadecimal lexer
octal           = P.octal lexer

symbol          = P.symbol lexer
lexeme          = P.lexeme lexer
whiteSpace      = P.whiteSpace lexer    

parens          = P.parens lexer    
braces          = P.braces lexer    
brackets        = P.brackets lexer
squares         = P.squares lexer

semi            = P.semi lexer
comma           = P.comma lexer
colon           = P.colon lexer
dot             = P.dot lexer
semiSep         = P.semiSep lexer  
semiSep1        = P.semiSep1 lexer    
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer

ellipsis = symbol "..."

-----------------------------------------------------------
-- Literals
-----------------------------------------------------------
literal =
    do v <- intLiteral <|> floatLiteral <|> chrLiteral <|> strLiteral
       return v
    <?> "literal"

intLiteral  = do n <- natural
		 return (IntLit n)
floatLiteral  = do n <- float
		   return (FloatLit n)
chrLiteral  = do c <- charLiteral
		 return (CharLit c)
strLiteral  = do s <- stringLiteral
		 return (StringLit s)

run :: Show a => Parser a -> String -> IO ()
run p input
    = case (parse p "" input) of
			      Left err -> do { putStr "parse error at"
					       ; print err
					       }
			      Right x -> print x

runLex :: Show a => Parser a -> String -> IO ()
runLex p input
        = run (do whiteSpace
                  x <- p
                  eof
                  return x
                 ) input

{-
-- other terminals

typeName = identifier
namespaceName = identifier
statementName = identifier
destructorName = identifier
templateID = identifier
templateType = identifier

nestedName = identifier
fullName = identifier
nestedNameStar = identifier
fullNameStar = identifier

hashIf = do symbol "#"
	    symbol "if"

hashElif = do symbol "#"
	    symbol "elif"

hashEndif = do symbol "#"
	    symbol "endif"

hashEndif = do symbol "#"
	    symbol "pragma"
-}
