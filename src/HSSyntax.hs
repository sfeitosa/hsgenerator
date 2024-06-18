module HSSyntax where

import Data.List (intercalate)

data Module = Module String [Import] [Declaration]

newtype Import = Import String deriving Show

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
          | TypeFunction [Type] Type
          | TypeAlgebraic String [Constructor]
          deriving Eq

data Pattern = VarPattern String
             | WildcardPattern
             | ConstructorPattern String [Pattern]
             | LiteralPattern Literal
             | TuplePattern [Pattern]
             | EmptyListPattern
             | ConsPattern Pattern Pattern
             deriving Eq

data Declaration = FunctionDecl String [Type] [Pattern] Type Expression
                 | DataDecl String [Constructor]
                 | TypeDecl String Type
                 deriving Eq

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
                | FunExpr String
                | AppExpr Expression [Expression]
                | LambdaExpr [Type] [Pattern] Expression
                | LetExpr String Expression Expression
                | CaseExpr Expression [CaseAlternative]
                | IfExpr Expression Expression Expression
                | TupleExpr [Expression]
                | ListExpr [Expression]
                | ConstructorExpr String [Expression]              
                deriving Eq

type Ctx = [(String, Type)]

type UT = [Type]

-------------------------------------------------------------------------------
-- Pretty Printing                                                           --
-------------------------------------------------------------------------------

instance Show Module where 
    show (Module n imps decls) = "module " ++ n ++ " where\n" ++ unwords (map show imps) ++ "\n" ++ intercalate "\n" (map show decls)

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
    show (TypeTuple ts) = "(" ++ intercalate "," (map show ts) ++ ")"
    show (TypeList t) = "[" ++ show t ++ "]"
    show (TypeFunction ts tr) = "(" ++ intercalate " -> " (map show ts) ++ " -> " ++ show tr ++ ")"
    show (TypeAlgebraic n cs) = n   

instance Show Literal where 
    show (IntLiteral i) | i < 0     = "(" ++ show i ++ ")"
                        | otherwise = show i
    show (IntegerLiteral i) | i < 0     = "(" ++ show i ++ ")"
                            | otherwise = show i
    show (FloatLiteral f)   = show f
    show (DoubleLiteral d)  = show d
    show (StringLiteral s)  = show s
    show (BoolLiteral b)    = show b
    show (CharLiteral c)    = show c

instance Show Constructor where 
    show (Constructor n ts) = n ++ " " ++ unwords (map show ts)  

instance Show Declaration where
    show (FunctionDecl n _ ps _ e) = -- n ++ " :: " ++ unwords (map show ts) ++ "\n" ++
                                    n ++ " " ++ unwords (map show ps) ++ " = " ++ show e ++ "\n"
    show (DataDecl n cs) = "data " ++ n ++ " = " ++ intercalate " | " (map show cs) ++ " deriving Show\n"
    show (TypeDecl n t) = "type " ++ n ++ " = " ++ show t ++ "\n"

instance Show Pattern where 
    show (VarPattern v) = v
    show WildcardPattern = "_"
    show (ConstructorPattern n ps) = "(" ++ n ++ " " ++ unwords (map show ps) ++ ")"
    show (LiteralPattern l) = show l
    show (TuplePattern ps) = "(" ++ intercalate "," (map show ps) ++ ")"
    show EmptyListPattern = "[]"
    show (ConsPattern p1 p2) = "(" ++ show p1 ++ ":" ++ show p2 ++ ")"

instance Show CaseAlternative where 
    show (CaseAlternative p e) = show p ++ " -> (" ++ show e ++ ");"

instance Show Expression where 
    show (LiteralExpr l) = show l
    show (VarExpr v)     = v
    show (FunExpr f)     = f
    show (AppExpr e [])  = show e
    show (AppExpr e1 [e2]) = show e1 ++ " (" ++  show e2 ++ ")"
    -- show (AppExpr e es)  = show e ++ " " ++ unwords (map show es)
    show (AppExpr e es) = show e ++ " (" ++ intercalate ") (" (map show es) ++ ")"
    show (LambdaExpr _ ps e) = "(\\" ++ unwords (map show ps) ++ " -> " ++ show e ++ ")"
    show (LetExpr v e1 e2) = "let " ++ v ++ " = " ++ show e1 ++ "" ++ "  in " ++ show e2
    show (CaseExpr e as) = "case (" ++ show e ++ ") of " ++ unwords (map show as)
    show (IfExpr e1 e2 e3) = "if (" ++ show e1 ++ ") then " ++ show e2 ++ " else " ++ show e3
    show (TupleExpr es) = "(" ++ intercalate "," (map show es) ++ ")"
    show (ListExpr es) = "[" ++ intercalate "," (map show es) ++ "]"
    show (ConstructorExpr n []) = n
    show (ConstructorExpr n es) = "(" ++ n ++ " (" ++ intercalate ") (" (map show es) ++ ")" ++ ")"
