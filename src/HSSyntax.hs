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
              deriving (Show, Eq)

data Constructor = Constructor String [Type]
                 deriving (Show, Eq)

data Type = TypePrim PrimType
          | TypeTuple [Type]
          | TypeList Type
          | TypeFunction Type Type
          | TypeAlgebraic [Constructor]
          deriving (Show, Eq)

data Pattern = VarPattern String
             | WildcardPattern
             | ConstructorPattern String [Pattern]
             | LiteralPattern Literal
             | TuplePattern [Pattern]
             | EmptyListPattern
             | ConsPattern Pattern Pattern
             deriving (Show, Eq)

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
             deriving (Show, Eq)

data CaseAlternative = CaseAlternative Pattern Expression
                     deriving (Show, Eq)

data Expression = LiteralExpr Literal
                | VarExpr String
                | FunExpr String
                | AppExpr Expression [Expression]
                | LambdaExpr [Type] [Pattern] Expression
                | LetExpr String Expression Expression
                | CaseExpr Expression [CaseAlternative]
                | IfExpr Expression Expression Expression
                | TupleExpr [Expression]
                | ListExpr [Expression]                
                deriving (Show, Eq)

type Ctx = [(String, Type)]