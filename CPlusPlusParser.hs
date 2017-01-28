-- Subset of C++ adapted from a PCCTS-based C++ grammar
-- This parser is *not* complete
-- It is used just to parse some header files
--
-- Eray Ozkural (exa) <erayo@cs.bilkent.edu.tr>, 2002
-- See COPYING file that comes with this distribution
--

module CPlusPlusParser
where
import Parsec
import ParsecExpr
import CPlusPlusLexer
import CPlusPlus
import Monad

-----------------------------------------------------------
-- A program is a list of external declarations
----------------------------------------------------------- 
-- Also called a translation unit, blah

program = many declarationExternal

-----------------------------------------------------------
-- Declarations
-----------------------------------------------------------

-- declaration_external:  Match an external declaration.
declarationExternal = try (
    declarationExtOrMember False
--    <|> asmDeclaration
    <|> do l <- linkageSpec
	   return l )
    <?> "external declaration"

-- declaration_external_or_member:  Matches common elements of 
-- declaration_member and declaration_external.
-- removed template declaration
--  bool    memberDecl     true if the declaration is inside a class

declarationExtOrMember :: Bool -> Parser String
declarationExtOrMember memberDecl =
    do declaration_SC_or_DM_Seq
       option "" (do ctorDeclarator
	             do ctorInitializer
	                if (memberDecl) then genericBlock
                          else statementCompound
                     <|> semi)
       <|> do option [] declarationSpecifiers
	      (semi <|> declaratorFunctionorInitList)
	      
-- declaration_member.  Matches declarations that are allowed
-- in struct/class/union.
declarationMember = 
    declarationExtOrMember True
    <|> do accessSpecifier
	   colon
    <|> pragmaLine

-- cv_qualifier.  Matches one of 'const' or 'volatile'.

cvQualifier = 
    do reserved "const"
       return Const
    <|> do reserved "volatile"
	   return Volatile
    <?> "const/volatile qualifier"

cvQualifierSeq = many cvQualifier

-- storage_class_specifier.

storageClassSpecifier =
    choice (map doSpec declWords)
    <?> "storage class specifier"
	where doSpec x = do reserved x
			    return (StorageClass x)
	      declWords = ["auto", "register", "static", "extern", "typedef"]

-- declaration_modifier: match any one of the possible
-- declaration modifiers.  Semantic checks must be done to
-- determine validity of the modifier with a particular declaration.
declarationModifier =
    choice (map doSpec declWords)
    <?> "declaration modifier"
	where doSpec x = do reserved x
			    return (DeclarationModifier x)
	      declWords = ["inline", "virtual", "friend", "explicit"]

declaration_SC_or_DM_Seq =
    many1 ( storageClassSpecifier <|> declarationModifier )

declarationSpecifiers =
    do many cvQualifier
       do builtinTypeSpecifier
	  many (builtinTypeSpecifier
		<|> declarationSpecifiersQualifier)
       <|> do (    qualifiedType
	       <|> classSpecifier
	       <|> enumSpecifier)
	      many declarationSpecifiersQualifier

-- declaration_specifiers_qualifier.  Match the part of the 
-- declaration specifiers that is just a qualifier
declarationSpecifiersQualifier =
    storageClassSpecifier
    <|> cvQualifier
    <|> declarationModifier

-- Rule: declaration_specifiers_simple.  Match the part of the declaration
-- that specifies the fundamental type of the declaration.  Like 
-- declaration_specifiers, but does not allow enums or classes with
-- explicit bodies like "enum {...}" or "class {...}"
declarationSpecifiersSimple =
    do many cvQualifier
       do builtinTypeSpecifier
	  many (builtinTypeSpecifier
		<|> declarationSpecifiersQualifier)
       <|> do (    qualifiedType
	       <|> classSpecifierSimple
	       <|> enumSpecifierSimple)
	      many declarationSpecifiersQualifier

-- Rule: qualified_id.  Matches a qualified identifier ::T::B::foo
-- (including dtor, "operator").  If allowType is true, then this 
-- will also match type names and ctors.  If allowCtor is true then 
-- it will match ctors.  dtors are always matched.

qualifiedId :: Bool -> Parser String
qualifiedId allowType =
--allowCtor allowAnyDtor =
    do when allowType (option () (reserved "typename"))
       scopeOverride
       do isDtor <- option False (do symbol "~"
				     return True )
  	  id <- identifier
	  return ""
       <|> do reserved "operator"
              (operSimple <|> operConversion)



-----------------------------------------------------------
-- Types
-----------------------------------------------------------

-- Rule: builtin_type_specifier.  Matches one of the built-in type
--      specifiers (int/float/bool/etc).
builtinTypes = ["char", "short", "int", "long", "signed"
	      , "unsigned", "float", "double", "void"]
builtinTypeSpecifier =
    choice (map doType builtinTypes)
    <?> "simple type name"
    where doType x = do reserved x
			return (BuiltinType x)

-- Rule: qualified_type.  Matches a type, optionally qualified by
-- scope-resolution prefixes.
qualifiedType =
    do reserved "typename"
       option "" scopeOverride
       identifier
       return (MyType "")

scopeOverride = do sepBy1 identifier (symbol "::")
		   symbol "::"

simpleTypeSpecifier =
    builtinTypeSpecifier <|> qualifiedType

-- Rule: enum_specifier.  Matches an enum-type specifier, which may
--      be a declaration, definition, or simply "enum E", where E is
--      a previously-declared enum.  For example:
--            enum E ;       enum declaration
--            enum E ...     previously-declared enum
--            enum E {...}   enum definition
enumSpecifier =
   do reserved "enum"
      ( do braces enumeratorList
	   return (Enum "")
	<|> do scopeOverride
	       e <- identifier
               braces enumeratorList
	       return (Enum e) )
-- or some tricky shit
   <?> "enum specifier"

-- Rule: enum_specifier_simple.  Matches an enum-type specifier, without
-- allowing a definition (i.e., {...}).
enumSpecifierSimple =
   do reserved "enum"
      scopeOverride
      e <- identifier
      return (Enum e)
   <?> "enum specifier simple"

-- Rule: enumerator.  Matches an enumerator for an enumeration.
--     Enters the enumerator as a declaration of the enumeration type.
enumerator =
    do identifier
       option "" (do symbol "="
		     exprConstant)
    <?> "enumerator"

enumeratorList = sepBy1 enumerator comma
	   <?> "enumerator list"

-- Rule: linkage_specification.  Matches stuff like
--    extern "C" declaration
-- or 
--    extern "C" { declarations }
linkageSpec =
      do reserved "extern"
	 strLiteral -- like "C"
	 braces declarationExternal
	 return ""
--	 return ExternLinkage
  <|> do reserved "extern"
	 strLiteral
         declarationExternal
	 return ""
--	 return ExternLinkage
  <?> "linkage specification"

asmDeclaration =
    do reserved "asm"
       str <- parens strLiteral
       semi
       return (AsmDecl "")
       <?> "assembly declaration"

-----------------------------------------------------------
-- Declarators
-----------------------------------------------------------

-- Rule: declarator.  Matches a named declarator.
declarator = try (
    -- Backtrack to differentiate between normal declaration
    -- and ptr-to-member qualifier on declaration:
    --    A::foo    // normal decl
    --    A::* foo  // ptr-to-member qualifier precedes decl
    -- Since ptr-to-member is rare, make it the second alt.
    option "" declaratorDirect
    <|> do declaratorPtr
           declarator
		 )

-- declarator_direct.  Matches a named declarator without prefixes.
-- This will match type names to handle odd cases like:
--    typedef struct S { } S;
-- This will match ctors to handle ctor declarations.
declaratorDirect =
    do qualifiedId False		-- must allow types and ctors
       declaratorSuffixes
    <|> do parens declarator
	   declaratorSuffixes
	   -- merge types here

-- Parenthesized declarators are hard for LL parsers,
-- because we really need the type modification resulting
-- from the suffixes before parsing the sub-declarator.
-- Instead, we fake it by starting with a fake type entry,
-- applying the sub-declarator modifiers to it, and
-- then re-applying the collected type modifiers to the
-- "real" suffix-modified type.
--
-- To further complicate matters, both the nested declarator
-- and the declarator_suffixes may declare a function suffix,
-- as in:
--     int (*foo())(int)  // function returning pointer to function
-- The rule to use here is that if the nested declarator is
-- "nil", e.g.:
--     int (foo)(int)
-- then use the exterior arguments, otherwise use the nested arguments.

-- Rule: declarator_function_or_init_list.  Matches either a single
-- declarator with a function body, a list of optionally-initialized
-- declarators, or a single semicolon
-- Note: We separate an initialized declarator
-- list from a function definition.  The reason is that
-- combining them yields wierd ambiguous situations like:
--    int foo() = new enum E { enumerators } 
-- The ambiguity is betweeen this interpretation and 
--    int foo() = new enum E { function body} 
-- To avoid this problem, we syntactically exclude something
-- from being both initialized and having a function body.
declaratorFunctionorInitList =
    do declarator
       do
          ( (do symbol "=" -- Optional initializer for the first declarator 
	        initializer)
	    <|> parens expressionList)
          comma -- Process more optional init-declarators
	  declaratorInitList
	  semi
       <|> do --  Declarator with function body
           -- Choose statement_compound for external declarations, but use
           -- generic_block for member declarations because the fields have
           -- not all been seen yet.
	     try (
		  genericBlock
		  <|> statementCompound
		 )

-- Rule declarator_init.  Matches a declarator with initializers, and
-- enters the declaration in the current scope.
declaratorInit =
    do declarator
       initializer
       <|> do parens expressionList

-- Rule: declarator_init_list.  Matches a list of optionally-initialized
-- declarators.
declaratorInitList =
    many (do comma
	     declaratorInit)

-- Rule: declarator_opt_abstract. A declarator that is optionally an 
-- abstract declarator (i.e., one with no declarator TOK_ID).  
-- Used for function parameters.
declaratorOptAbstract =
    try (do declaratorPtr
	     declaratorOptAbstract
	  <|> do parens declaratorOptAbstract
	         option () ( declaratorSuffixFunction
			     <|> declaratorSuffixArray )
              <|> identifier
	          ( declaratorSuffixFunction
		    <|> declaratorSuffixArray)
	  <|> declaratorSuffixArray
	  <|> return ()
	)
-- declarator_ptr:  Matches a pointer or reference.
declaratorPtr =
    do 
      (symbol "&"
       <|> symbol "*"
       <|> declaratorPtrToMember)
      cvQualifierSeq

-- declarator_ptr_to_member.  Match T::TT::*
declaratorPtrToMember =
    do symbol "::"
       symbol "*"

-- Rule: declarator_suffix_array.  Matches array suffixes on a
-- declarator, like "[ expr ]"
declaratorSuffixArray =
    do squares exprConstant
       many (squares (exprConstant <|> return ""))

-- Rule: declarator_suffix_bitfield.  Matches bitfield suffixes on a
-- declarator ( : expr ).
declaratorSuffixBitfield =
    do colon
       exprConstant

-- Rule: declarator_suffix_function.  Matches argument suffixes on a
-- declarator (arg, arg, ...)
declaratorSuffixFunction =
    do parens parameterListOpt
       cvQualifierSeq

-- Rule: declarator_suffixes.  general-purpose declarator suffixes.
-- Not intended for abstract declarators.
declaratorSuffixes =
    do declaratorSuffixArray
       return ""
    <|> do option [] declaratorSuffixFunction
	   return ""
    <|> do declaratorSuffixBitfield
	   return ""
    <|> return ""

-- Rule: parameterDeclaration.  Matches an optionally-assignment-
-- initialized declaration for a function argument.
parameterDeclaration =
    do declaration_SC_or_DM_Seq
       declarationSpecifiersSimple
       declaratorOptAbstract
       option "" (do symbol "="
		     expression)

-- Rule: parameter_list.  Matches a comma-separated list of optionally-
-- assignment-initialized declarations, with an optional trailing ellipses.
parameterList =
    commaSep1 parameterDeclaration
    option () (do option () comma
	          ellipsis)
    <|> ellipsis

-- Rule: parameter_list_opt.  Matches a an optional parameter_list
parameterListOpt = 
    option () (reserved "void")
    <|> parameterList
		     
-- Rule: initializer.  Matches an initialization expression or
-- list of expressions in curly braces.
initializer=
    do symbol "="
       expression
       <|> braces (commaSep1 expression)
    <?> "initializer"

-- eat funcbody here
--funcbody = compoundStmt

funcbody = genericBlock
genericBlock = braces genericBlock <|> simpleBlock
simpleBlock = braces (many (noneOf "{}"))

-- Names

enumName = identifier
className = identifier
typeDefName = identifier

--operator = 
--      <|> {operator}

-----------------------------------------------------------
--  Class Declarations
-----------------------------------------------------------

classSpecifier =
    do reserved "class"
       return emptyClass

classSpecifierSimple =
    do reserved "class"
       return emptyClass

-- Rule ctor_declarator:  Matches the declarator for a constructor,
--    which is like a normal function declarator. but has an optional
--    trailing member initializer list.
ctorDeclarator =
    do qualifiedId False
       declaratorSuffixFunction
    <|> parens ctorDeclarator

-- Rule ctor_initializer:  Matches the initializer list for a constructor.
ctorInitializer =
    do colon
       commaSep1 ctorMemberInit

-- Rule: ctor_member_init.  Matches a member initializer for a
-- constructor, like 
--   A::A() : 
--      x(1)   <-- member initializer
ctorMemberInit =
    qualifiedId True
    parens expression

-- access_specifier.  Matches one of public/protected/private.
accessSpecifier =
    do reserved "private"
       return Private
    <|> do reserved "protected"
	   return Protected
    <|> do reserved "private"
	   return Private

-- Rule: optor_simple.  Match any of the stock operator types, but 
-- not the user-defined type conversion operators.
operSimple = 
      reserved "new"
      option False squares (return True)
      <|> reserved "delete"
	  option False squares (return True)
      <|> choice (map doOp opNames)
      <?> "operator"
    where doOp x = do reservedOp x
		      return ()

-- Rule: optor_conversion.  Match a user-defined type conversion specifier
-- after an "operator" token.
operConversion =
    declarationSpecifiersSimple
    -- // Disallow storage class of decl modifier
    many (declaratorPtr)

-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------    

exprConstant = expression
expressionList = expression

--expression :: Parser Expr      
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
      op name assoc   = Infix (do var <- try (reservedOp name)
                                  return (\x y -> "") 
                                  ) assoc
      prefix name     = Prefix (do var <- try (symbol name)
				   return (\x -> ""))
                                                                   
--       op name assoc   = Infix (do{ var <- try (symbol name)
--                                   ; return (\x y -> App (App (Var [var]) x) y) 
--                                   }) assoc
--       prefix name     = Prefix  (do{ var <- try (symbol name)
--                                   ; return (\x -> App (Var [var,"unary"]) x)
--                                   })                                  
                                                
applyExpression =
    do exprs <- many1 simpleExpression
       return ""
 
newExpression = do reserved "new"
		   return ""
      
--simpleExpression :: Parser Expr    
simpleExpression =
    do intLiteral
       return ""
    <|> parens expression
--     <|> caseExpression
--     <|> variable            
    <?> "simple expression"


-----------------------------------------------------------
--  Statements
-----------------------------------------------------------

statementCompound = genericBlock

-----------------------------------------------------------
--  Preprocessor
-----------------------------------------------------------

-- Rule: pragma_line.  Matches #pragma "line" forwarded from preprocessor
-- incomplete here
pragmaLine =
    symbol "#pragma"
       