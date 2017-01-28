declarationInt = try (
       do declSpec
	  declaratorList
	  semi
	  return ""
       <|> asmDeclaration
       <|> do l <- linkageSpec
	      return l )
    <?> "internal declaration"

declSpec =
    do t <- typeSpec
       return ""
    <|> choice (map doSpec declWords)
    <?> "declaration specification"
	where doSpec x = do reserved x
			    return x
	      declWords = ["friend", "typedef", "auto", "register",
			   "static", "extern", "inline","virtual"]

typeSpec =
    do reserved "enum"
       e <- enumName
       return (Enum e)
    <|> do reserved "const"
	   return Const
    <|> do reserved "volatile"
	   return Volatile
    <|> enumSpec
    <|> classSpec
    <|> simpleTypeName
    <?> "type specification"

simpleTypes = ["char", "short", "int", "long", "signed"
	      , "unsigned", "float", "double", "void"]


simpleTypeName =
    do  id <- className
        return (ClassRef id)
    <|> do id <- typeDefName
	   return (TypeDef id)
    <|> choice (map doType simpleTypes)
    <?> "simple type name"
    where doType x = do reserved x
			return (SimpleType x)

classKey =
        reserved "class"
    <|> reserved "struct"
    <|> reserved "union"
    <?> "class key (class/struct/union)"

-----------------------------------------------------------
-- Declarators
-----------------------------------------------------------

declaratorList = commaSep initDeclarator
		 <?> "list of declarators/initializers"
initDeclarator =
    do declarator
       option "" initializer
    <?> "declarator/initializer"

-- avoid left recursion!
declaratorTerm = try (
    name
    <|> do ptrOper
	   declarator
    <|> parens declarator
    <?> "declarator"
    )


-- declarator.  Matches a named declarator.

{-
declarator =
    declaratorTerm
    <|> do chainl1 declaratorTerm f
	   do parens argDeclarationList
	      option "" cvQualifierList
	      return ""
    <?> "chain of declarators"
    where f = return (++)

declarator = try (
        name
    <|> do ptrOper
	   declarator
--    <|> do declarator
--	   squares (option constantExp)
    <|> parens declarator
		     )

declaratorTail =
    do parens argDeclarationList
       option "" cvQualifierList
       return ""
-}

-- pointer operator, verified
ptrOper =
      do symbol "*"
	 option "" cvQualifierList
      <|> do symbol "&"
	     option "" cvQualifierList
      <|> do className
	     symbol "::"
	     symbol "*"
	     option "" cvQualifierList
      <?> "pointer operator"


typeName =
    do t <- typeSpec
       return ""
    <|> do t <- typeSpec
	   absDeclarator
    <?> "type name"

-- abstract declarator

-- eliminate left recursion!

{-
absDeclarator = try (
        ptrOper
    <|> do ptrOper
	   absDeclarator
--     <|> do absDeclarator
-- 	   parens argDeclarationList
-- 	   option "" cvQualifierList
--     <|> do {absDeclarator; squares (option constantExp)}
    <|> do parens argDeclarationList
           option "" cvQualifierList
--     <|> squares (option constantExp)
    <|> parens absDeclarator 
)
-}

absDeclaratorSimple =
    try (
	 ptrOper
	 <|> do ptrOper
	        absDeclarator
	 <|> do parens argDeclarationList
                option "" cvQualifierList
--     <|> squares (option constantExp)
	 )

absDeclarator =
    chainl1 absDeclaratorSimple f
    <|> parens absDeclarator
    <?> "abstract declarator"
    where f = return (++)

argDeclarationList = try (
    do symbol "..."
       return []
   <|> commaSep1 argDeclaration
   <|> do commaSep1 argDeclaration
	  symbol "..."
	  return []
   <|> do commaSep1 argDeclaration
	  comma
	  symbol "..."
	  return []
			 )

{-
argDeclaration = try (
        do d <- many1 declSpec
	   do declarator
	      option "" (do symbol "="
			    expression)
	   <|> do option "" absDeclarator
	          symbol "="
	          expression
	)
-}

argDeclaration = try (
        do d <- many1 declSpec
	   do option "" absDeclarator
	      option "" (do symbol "="
		            expression)
	)
        <?> "argument declaration"

funcDefinition = try (
    do many1 declSpec
       declarator
       option "" ctorInit
       funcbody
       return (FuncDef "")
    <|> do declarator
	   option "" ctorInit
	   funcbody
	   return (FuncDef "")
    <?> "function definition"

initList =
      expression
      <|> do initList
	     comma
	     expression
      <|> braces (do initList
		     option "" comma)
      <?> "initializer list"

initializer=
    do symbol "="
       expression
    <|> do symbol "="
           braces (do initList
		      option "" comma)
    <|> parens expression
    <?> "initializer"

-----------------------------------------------------------
--  Class Declarations
-----------------------------------------------------------

classSpec :: Parser TypeSpec
classSpec =
    do classKey
       try (
	    do braces (option [] memberList)
	       return ( Const )
	    <|> do colon
  	           commaSep1 baseSpec
                   braces (option [] memberList)
	           return ( Const )
	    <|> do className
 	           braces (option [] memberList)
	           return ( Const )
	    <|> do className
	           colon
	           commaSep1 baseSpec
	           braces (option [] memberList)
	           return ( Const )
	    <|> do identifier
	           braces (option [] memberList)
	           return ( Const )
	    <|> do id <- identifier
	           colon
	           commaSep1 baseSpec
	           m <- braces (option [] memberList)
	           return (Class id m)
--	           return (Class {cname = identifier, members = m})
	    <|> do id <- identifier
	           return (ClassRef id)
	   )
    <?> "class specification"

memberListComponent :: Parser MemberListComp
memberListComponent =
    do l <- many1 memberDeclaration
       return (NoSpec, l)
    <|> do a <- accessSpec
	   colon
	   l <- many1 memberDeclaration
	   return (a, l)

memberList :: Parser Members
memberList = do l <- many1 memberListComponent
		return l
		<?> "member list"

memberDeclaration :: Parser Member
memberDeclaration = try (
    do semi
       return NoMember;
    <|> do f <- funcDefinition
	   return (MemberFunction "")
    <|> do f <- funcDefinition
	   semi
	   return (MemberFunction "")
    <|> do qualName
	   semi
	   return (MemberName)
    <|> do memberDeclaratorList
	   semi
	   return (MemberDeclList)
    <|> do many1 declSpec
	   semi
           return (MemberName)
    <|> do many1 declSpec
	   memberDeclaratorList
	   semi
           return (MemberDeclList)
    )
    <?> "member declaration"

memberDeclaratorList = many1 memberDeclarator
		       <?> "member declarator list"

memberDeclarator =
    declarator
    <|> do declarator
	   pureSpec
--     <|> do identifier
-- 	   symbol ':'
-- 	   constantExp
--     <|> do symbol ':'
-- 	   constantExp
    <?> "member declarator"

-- fixme
pureSpec =
      do symbol "="
	 symbol "0"
      <?> "pure specifier"

baseSpec =
    className
    <|> do reserved "virtual"
           option Private accessSpec
	   className
    <|> do accessSpec
	   option () (reserved "virtual")
	   className
    <?> "base specifier"

{-
accessSpec = choice (map doAccess ["private","protected","public"])
	     <?> "access specifier"
	     where doAccess x = do reserved x
				   return x
-}

-- access_specifier.  Matches one of public/protected/private.

accessSpec =
    do reserved "private"
       return Private
    <|> do reserved "protected"
	   return Protected
    <|> do reserved "private"
	   return Private

convFuncName = do reserved "operator"
		  convTypeName
		  return "convFuncName"

convTypeName =
    do t <- typeSpec
       return ""
    <|> do typeSpec
	   ptrOper

ctorInit =
    do symbol ":"
       memInitList
       return "ctorInit"

memInitList = sepBy1 memInit comma

memInit =
    do className
       parens (option "" expression)
    <|> do identifier
	   parens (option "" expression)

operFuncName =
    do reserved "operator"
       oper
       return "operFuncName"

oper = 
      reserved "new"
      <|> reserved "delete"
      <|> choice (map doOp opNames)
      <?> "operator"
    where doOp x = do reservedOp x
		      return ()

name :: Parser String
name = try (
    identifier
    <|> operFuncName
    <|> convFuncName
    <|> qualName )
    <?> "name"

qualName =
      do className
	 symbol "::"
	 identifier
      <|> do className
	     symbol "::"
	     operFuncName
      <|> do className
	     symbol "::"
	     convFuncName
      <|> do className
	     symbol "::"
	     className
      <|> do className
	     symbol "::"
	     symbol "~"
	     className
      <?> "qualified name"

