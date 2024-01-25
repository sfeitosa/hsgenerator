module HSGenExpression where 
    
import Test.QuickCheck
import Data.Maybe
import Consts
import Utils
import HSSyntax
import HSGenPattern
import HSGenType
import HSGenDeclaration
import Control.Monad

-------------------------------------------------------------------------------
-- Generating Literal Expression                                             --
-------------------------------------------------------------------------------

genLiteral :: PrimType -> Gen Literal
genLiteral IntType     = IntLiteral <$> arbitrary
genLiteral IntegerType = IntegerLiteral <$> arbitrary
genLiteral FloatType   = FloatLiteral <$> arbitrary
genLiteral DoubleType  = DoubleLiteral <$> arbitrary
genLiteral StringType  = StringLiteral . getPrintableString <$> arbitrary
genLiteral BoolType    = BoolLiteral <$> arbitrary
genLiteral CharType    = CharLiteral <$> arbitraryPrintableChar

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

genAppExpr :: Int -> Ctx -> Type -> Gen Expression
genAppExpr s ctx t = 
    let fs = [(n, tp) | (n, TypeFunction tp tr) <- ctx, tr == t]
      in if null fs then 
           do 
             tp <- genType s
             e  <- genLambdaExpr (s `div` 2) ctx (TypeFunction tp t)
             es <- mapM (genExpression (s `div` 2) ctx) (getLambdaParamTypes e)
             return $ AppExpr e es
         else 
           do
             f  <- elements fs
             es <- mapM (genExpression (s `div` 2) ctx) [snd f] -- @TODO: generate more than one argument
             return $ AppExpr (VarExpr (fst f)) es

-------------------------------------------------------------------------------
-- Generating Lambda Expression                                              --
-------------------------------------------------------------------------------

genLambdaExpr :: Int -> Ctx -> Type -> Gen Expression -- @TODO: change to return Maybe
genLambdaExpr s ctx (TypeFunction tp tr) = do
    p  <- genPattern tp
    e  <- genExpression (s `div` 2) (getPatternVars tp p ++ ctx) tr
    return $ LambdaExpr [tp] [p] e

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

genAlt :: Int -> Ctx -> Type -> Gen CaseAlternative
genAlt s ctx t = do
    p <- genPattern t
    e <- genExpression (s `div` 2) (getPatternVars t p ++ ctx) t
    return $ CaseAlternative p e

genAlts :: Int -> Ctx -> Type -> Gen [CaseAlternative]
genAlts s ctx t = do
    mx <- chooseInt (0, maxCaseAltSize)
    vectorOf mx (genAlt s ctx t)

genCaseExpr :: Int -> Ctx -> Type -> Gen Expression
genCaseExpr s ctx t = do
    e  <- genExpression (s `div` 2) ctx t
    al <- genAlts (s `div` 2) ctx t
    return $ CaseExpr e al

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

genExprList :: Int -> Ctx -> Type -> Gen [Expression]
genExprList s ctx t = do
    mx <- chooseInt (0, maxListSize)
    vectorOf mx (genExpression (s `div` 2) ctx t)    

genListExpr :: Int -> Ctx -> Type -> Maybe (Gen Expression)
genListExpr s ctx (TypeList t) = Just $ ListExpr <$> genExprList s ctx t
genListExpr _ _ _              = Nothing

-------------------------------------------------------------------------------
-- Generating Expressions                                                    --
-------------------------------------------------------------------------------

genFinalExpression :: Ctx -> Type -> Gen Expression
genFinalExpression ctx t@(TypeTuple ts) = 
    let es = catMaybes [genTupleExpr 0 ctx t, 
                        genVarExpr ctx t]
      in do join (elements es)
genFinalExpression ctx t@(TypeList t') = 
    let es = catMaybes [genListExpr 0 ctx t, 
                        genVarExpr ctx t]
      in do join (elements es)
genFinalExpression ctx t@(TypePrim t') = 
    let es = catMaybes [genLiteralExpr t, 
                        genVarExpr ctx t]
      in do join (elements es)
genFinalExpression ctx t@(TypeFunction t1 t2) = 
    LambdaExpr [t1] <$> mapM genPattern [t1] <*> genExpression 0 ctx t2
-- genFinalExpression ctx t@(TypeFunction t1 t2) = 
--     let es = catMaybes [Just $ genLambdaExpr 0 ctx t, 
--                         genVarExpr ctx t]
--       in do join (elements es)
genFinalExpression ctx t = error $ "genFinalExpression for (" ++ show t ++ ") not implemented."

genRecursiveExpression :: Int -> Ctx -> Type -> Gen Expression
genRecursiveExpression s ctx t = 
    let es1 = catMaybes [genLiteralExpr t, 
                         genVarExpr ctx t, 
                         genTupleExpr (s `div` 2) ctx t,
                         genListExpr (s `div` 2) ctx t]
        es2 = [genAppExpr s ctx t, 
               genLambdaExpr s ctx t, 
               genLetExpr s ctx t, 
               genCaseExpr s ctx t, 
               genIfExpr s ctx t]         
      in do join (elements (es1 ++ es2))

genExpression :: Int -> Ctx -> Type -> Gen Expression
genExpression s ctx t | s > 0 = genRecursiveExpression (s `div` 2) ctx t
                      | otherwise = genFinalExpression ctx t
