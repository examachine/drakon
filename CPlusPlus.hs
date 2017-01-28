-- C++ Semantics

module CPlusPlus
where

{-
ACCESS ;
ACCESSES ;
BOOL ;
BTYPE ;
CONDITION ;
COUNT ;
CV ;
DECL ;
DSPEC ;
EXP ;
IDENTIFIER ;
KEY ;
LEX ;
LINKAGE ;
LIST-EXP ;
LIST-TYPE ;
NAMESPACE ;
NUMBER ;
OFFSET ;
QUALIFIER ;
TEMPLATE ;
TYPE ;
-}

data Decl
    = AsmDecl String
    | FuncDecl
    | ExternLinkage
    | DeclaratorList
    deriving Show

data Def
    = FuncDef String
    deriving Show

data TypeSpec
    = Enum String
    | ClassRef String
    | Class {cname :: String, members :: Members }
    | Const
    | Volatile
    | TypeDef String
    | BuiltinType String
    | Void
    | StorageClass String
    | DeclarationModifier String
    | MyType String
    deriving Show

emptyClass = Class {cname = "", members = []}

data AccessSpec
    = NoSpec | Private | Public | Protected
    deriving Show

type MemberListComp = (AccessSpec, [Member])

type Members = [ MemberListComp ]

data Member
    = MemberFunction String
    | MemberVariable String
    | MemberName
    | MemberDeclList
    | NoMember
    deriving Show

data Exp
    = Exp Lit
    deriving Show

data Lit
    = IntLit Integer
    | FloatLit Double
    | CharLit Char
    | StringLit String
    deriving Show
