module HSSyntax where

data Module = Module String [Import] [Declaration]

newtype Import = Import String

data PrimType = IntType
              | IntegerType
              | FloatType
              | DoubleType
              | StringType
              | BoolType
              | CharType
              deriving Eq

data Constructor = Constructor String [Type]
                 deriving Eq

data Type = TypePrim PrimType
          | TypeTuple [Type]
          | TypeList Type
          | TypeFunction Type Type
          | TypeAlgebraic [Constructor]
          deriving Eq

data Pattern = VarPattern String
             | WildcardPattern
             | ConstructorPattern String [Pattern]
             | LiteralPattern Literal
             | TuplePattern [Pattern]
             | EmptyListPattern
             | ConsPattern Pattern Pattern
             deriving Eq

data Declaration = FunctionDecl String [Type] [Pattern] Expression
                 | DataDecl String [Constructor]
                 | TypeDecl String Type
                 deriving (Show, Eq)

data Literal = IntLiteral Int
             | IntegerLiteral Integer
             | FloatLiteral Float
             | DoubleLiteral Double
             | StringLiteral String
             | BoolLiteral Bool
             | CharLiteral Char
             deriving Eq

data CaseAlternative = CaseAlternative Pattern Expression
                     deriving Eq

data Expression = LiteralExpr Literal
                | VarExpr String
                | AppExpr Expression [Expression]
                | LambdaExpr [Type] [Pattern] Expression
                | LetExpr String Expression Expression
                | CaseExpr Expression [CaseAlternative]
                | IfExpr Expression Expression Expression
                | TupleExpr [Expression]
                | ListExpr [Expression]                
                deriving Eq

type Ctx = [(String, Type)]

-------------------------------------------------------------------------------
-- Pretty Printing                                                           --
-------------------------------------------------------------------------------

instance Show PrimType where 
    show IntType     = "Int"
    show IntegerType = "Integer"
    show FloatType   = "Float"
    show DoubleType  = "Double"
    show StringType  = "String"
    show BoolType    = "Bool"
    show CharType    = "Char"

instance Show Type where 
    show (TypePrim t) = show t
    show (TypeTuple ts) = "(" ++ unwords (map show ts) ++ ")"
    show (TypeList t) = "[" ++ show t ++ "]"
    show (TypeFunction t1 t2) = show t1 ++ " -> " ++ show t2
    show (TypeAlgebraic cs) = "ALGEBRAIC => " ++ unwords (map show cs)

instance Show Constructor where 
    show (Constructor n ts) = n ++ " " ++ unwords (map show ts)     

instance Show Literal where 
    show (IntLiteral i)     = show i
    show (IntegerLiteral i) = show i
    show (FloatLiteral f)   = show f
    show (DoubleLiteral d)  = show d
    show (StringLiteral s)  = show s
    show (BoolLiteral b)    = show b
    show (CharLiteral c)    = show c

instance Show Pattern where 
    show (VarPattern v) = v
    show WildcardPattern = "_"
    show (ConstructorPattern n ps) = n ++ " " ++ unwords (map show ps)
    show (LiteralPattern l) = show l
    show (TuplePattern ps) = "(" ++ unwords (map show ps) ++ ")"
    show EmptyListPattern = "[]"
    show (ConsPattern p1 p2) = show p1 ++ ":" ++ show p2

instance Show CaseAlternative where 
    show (CaseAlternative p e) = show p ++ " -> " ++ show e ++ ";"

instance Show Expression where 
    show (LiteralExpr l) = show l
    show (VarExpr v)     = v
    show (AppExpr e es)  = show e ++ " " ++ unwords (map show es)
    show (LambdaExpr _ ps e) = "\\" ++ unwords (map show ps) ++ " -> " ++ show e
    show (LetExpr v e1 e2) = "let " ++ v ++ " = " ++ show e1 ++ "" ++ "  in " ++ show e2
    show (CaseExpr e as) = "case " ++ show e ++ " of " ++ unwords (map show as)
    show (IfExpr e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
    show (TupleExpr es) = "(" ++ unwords (map show es) ++ ")"
    show (ListExpr es) = "[" ++ unwords (map show es) ++ "]"
