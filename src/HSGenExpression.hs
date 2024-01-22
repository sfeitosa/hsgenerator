module HSGenExpression where 
    
import Test.QuickCheck
import Consts
import Utils
import HSSyntax
import HSGenType
import HSGenDeclaration
import Control.Monad

-------------------------------------------------------------------------------
-- Generating Literal Expression                                             --
-------------------------------------------------------------------------------

genLiteral :: PrimType -> Gen Literal
genLiteral IntType    = IntLiteral <$> arbitrary
genLiteral IntegerType = IntegerLiteral <$> arbitrary
genLiteral FloatType  = FloatLiteral <$> arbitrary
genLiteral DoubleType = DoubleLiteral <$> arbitrary
genLiteral StringType = StringLiteral . getPrintableString <$> arbitrary
genLiteral BoolType   = BoolLiteral <$> arbitrary
genLiteral CharType   = CharLiteral <$> arbitraryPrintableChar

genLiteralExpr :: Type -> Maybe (Gen Expression)
genLiteralExpr (TypePrim t) = Just $ LiteralExpr <$> genLiteral t
genLiteralExpr _            = Nothing

-------------------------------------------------------------------------------
-- Generating Var Expression                                                 --
-------------------------------------------------------------------------------

genVarExpr :: Ctx -> Type -> Maybe (Gen Expression)
genVarExpr ctx t = 
       let vs = [VarExpr v | (v, t') <- ctx, t == t']
         in if null vs then 
              Nothing 
            else 
              Just $ elements vs

-------------------------------------------------------------------------------
-- Generating Function Application Expression                                --
-------------------------------------------------------------------------------

genAppExpr :: Int -> Ctx -> Type -> Maybe (Gen Expression)
genAppExpr s ctx t = undefined

-------------------------------------------------------------------------------
-- Generating Lambda Expression                                              --
-------------------------------------------------------------------------------

genLambdaExpr :: Int -> Ctx -> Type -> Gen Expression
genLambdaExpr s ctx tr = do
    tp <- genType s -- @TODO: Will we allow more than one variable on lambdas?
    p  <- genPattern tp
    e  <- genExpression (s `div` 2) (getPatternVars tp p ++ ctx) tr
    return $ LambdaExpr [p] e

-------------------------------------------------------------------------------
-- Generating Let Expressions                                                --
-------------------------------------------------------------------------------

genLetExpr :: Int -> Ctx -> Type -> Gen Expression
genLetExpr s ctx t = do
    n  <- elements varNames -- @TODO: get and remove var name
    t  <- genType s
    e1 <- genExpression (s `div` 2) ctx t
    e2 <- genExpression (s `div` 2) ((n, t) : ctx) t
    return $ LetExpr n e1 e2

-------------------------------------------------------------------------------
-- Generating Case Expressions                                               --
-------------------------------------------------------------------------------

genCaseExpr :: Int -> Ctx -> Type -> Gen Expression
genCaseExpr s ctx t = undefined

-------------------------------------------------------------------------------
-- Generating If Expressions                                                 --
-------------------------------------------------------------------------------

genIfExpr :: Int -> Ctx -> Type -> Gen Expression
genIfExpr s ctx t = do
    e1 <- genExpression (s `div` 2) ctx (TypePrim BoolType)
    e2 <- genExpression (s `div` 2) ctx t
    e3 <- genExpression (s `div` 2) ctx t
    return $ IfExpr e1 e2 e3

-------------------------------------------------------------------------------
-- Generating Tuple Expressions                                              --
-------------------------------------------------------------------------------

genTupleExpr :: Int -> Ctx -> Type -> Maybe (Gen Expression)
genTupleExpr s ctx (TypeTuple tt) = 
    let es = mapM (genExpression (s `div` 2) ctx) tt
      in Just $ TupleExpr <$> es    
genTupleExpr _ _ _                = Nothing

-------------------------------------------------------------------------------
-- Generating List Expressions                                               --
-------------------------------------------------------------------------------

genListExpr :: Int -> Ctx -> Type -> Maybe (Gen Expression)
genListExpr s ctx t = undefined

-------------------------------------------------------------------------------
-- Generating Expressions                                                    --
-------------------------------------------------------------------------------

genExpression :: Int -> Ctx -> Type -> Gen Expression
genExpression s ctx t = undefined

-- genExpression :: Ctx -> Int -> Type -> Gen Expression
-- genExpression ctx s t | s > 0     = frequency [(100, genLiteralExpr t),
--                                                (10, genVarExpr ctx t)]
--                                         --  (10, genAppExpr s'), 
--                                         --  (10, genLambdaExpr s'), 
--                                         --  (10, genLetExpr s'), 
--                                         --  (10, genCaseExpr s'), 
--                                         --  (10, genIfExpr s'), 
--                                         --  (10, genTupleExpr s'), 
--                                         --  (10, genListExpr s'), 
--                                         --  (10, genConstructorExpr s')]
--                       | otherwise = genLiteralExpr t
--     where s' = s `div` 2
