-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------    
expression :: Parser Expr      
expression =
        newExpression 
    <|> infixExpression     
    <?> "expression"
    
-----------------------------------------------------------
-- Infix expression
-----------------------------------------------------------
infixExpression = 
    buildExpressionParser operators applyExpression
    
operators =
    [ [ prefix "-", prefix "+" ]
    , [ op "^"  AssocRight ]
    , [ op "*"  AssocLeft, op "/"  AssocLeft ]
    , [ op "+"  AssocLeft, op "-"  AssocLeft ]
    , [ op "==" AssocNone, op "/=" AssocNone, op "<"  AssocNone
      , op "<=" AssocNone, op ">"  AssocNone, op ">=" AssocNone ]
    , [ op "&&" AssocNone ]
    , [ op "||" AssocNone ]
    ]
    where
      op name assoc   = Infix (do{ var <- try (symbol name)
                                  ; return (\x y -> App (App (Var [var]) x) y) 
                                  }) assoc
      prefix name     = Prefix  (do{ var <- try (symbol name)
                                  ; return (\x -> App (Var [var,"unary"]) x)
                                  })                                  
                                                
  

applyExpression =
    do{ exprs <- many1 simpleExpression
      ; return (foldl1 App exprs)
      }
      
{-
infixExpression =
    do{ (e,es) <- chain simpleExpression operator "infix expression"
      ; return $ if null es then e else (unChain (Chain e es))
      }
-}
    
simpleExpression :: Parser Expr    
simpleExpression =
        literal
    <|> parens expression
    <|> caseExpression
    <|> variable            
    <?> "simple expression"
