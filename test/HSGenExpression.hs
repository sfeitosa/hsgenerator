module HSGenExpression where 
    
import Test.QuickCheck
import Data.Maybe
import Consts
import Utils
import HSSyntax
import HSGenPattern
import HSGenType
import Control.Monad

-------------------------------------------------------------------------------
-- Generating Literal Expression                                             --
-------------------------------------------------------------------------------

genLiteral :: PrimType -> Gen Literal
genLiteral IntType     = IntLiteral <$> arbitrary
genLiteral IntegerType = IntegerLiteral <$> arbitrary
genLiteral FloatType   = FloatLiteral <$> elements (map (\x -> fromIntegral (truncate (x * 100)) / 100) [1, 1.01 .. 10])
genLiteral DoubleType  = DoubleLiteral <$> elements (map (\x -> fromIntegral (truncate (x * 100)) / 100) [1, 1.01 .. 10])
genLiteral StringType  = StringLiteral <$> elements phrases
genLiteral BoolType    = BoolLiteral <$> arbitrary
genLiteral CharType    = CharLiteral <$> elements ['!'..'~']

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

genAppExpr :: Int -> UT -> Ctx -> Type -> Gen Expression
genAppExpr s ut ctx t = 
    let fs = [(n, tp) | (n, TypeFunction tp tr) <- ctx, tr == t]
      in if null fs then 
           do 
             tp <- genType s AllTypes ut -- @TODO: generate more than one argument
             e  <- fromJust $ genLambdaExpr (s `div` 2) ut ctx (TypeFunction [tp] t)
             es <- mapM (genExpression (s `div` 2) ut ctx) (getLambdaParamTypes e)
             return $ AppExpr e es
         else 
           do
             f  <- elements fs
             es <- mapM (genExpression (s `div` 2) ut ctx) (snd f)
             return $ AppExpr (FunExpr (fst f)) es

-------------------------------------------------------------------------------
-- Generating Lambda Expression                                              --
-------------------------------------------------------------------------------

genLambdaExpr :: Int -> UT -> Ctx -> Type -> Maybe (Gen Expression)
genLambdaExpr s ut ctx (TypeFunction tp tr) = Just $ do
    ps <- mapM (genPattern s General) tp
    e  <- genExpression (s `div` 2) ut (getListPatternVars tp ps ++ ctx) tr
    return $ LambdaExpr tp ps e
genLambdaExpr _ _ _ _                     = Nothing

-------------------------------------------------------------------------------
-- Generating Let Expressions                                                --
-------------------------------------------------------------------------------

genLetExpr :: Int -> UT -> Ctx -> Type -> Gen Expression
genLetExpr s ut ctx tr = do
    n  <- getNextName (s + length ut + length ctx) varNames -- @TODO: get and remove var name
    tv <- genType s AllTypes ut
    e1 <- genExpression (s `div` 2) ut ctx tv
    e2 <- genExpression (s `div` 2) ut ((n, tv) : ctx) tr
    return $ LetExpr n e1 e2

-------------------------------------------------------------------------------
-- Generating Case Expressions                                               --
-------------------------------------------------------------------------------

genAlt :: Int -> TPattern -> UT -> Ctx -> Type -> Type -> Gen CaseAlternative
genAlt s f ut ctx tm t = do
    p <- genPattern s f tm
    e <- genExpression (s `div` 2) ut (getPatternVars tm p ++ ctx) t
    return $ CaseAlternative p e

genCaseExpr :: Int -> UT -> Ctx -> Type -> Gen Expression
genCaseExpr s ut ctx t = do 
    tm <- genType (s `div` 2) MatchingTypes ut
    e  <- genExpression (s `div` 2) ut ctx tm
    a1 <- genAlt (s `div` 2) Specific ut ctx tm t 
    a2 <- genAlt (s `div` 2) General ut ctx tm t
    return $ CaseExpr e [a1, a2]

-------------------------------------------------------------------------------
-- Generating If Expressions                                                 --
-------------------------------------------------------------------------------

genIfExpr :: Int -> UT -> Ctx -> Type -> Gen Expression
genIfExpr s ut ctx t = do
    e1 <- genExpression (s `div` 2) ut ctx (TypePrim BoolType)
    e2 <- genExpression (s `div` 2) ut ctx t
    e3 <- genExpression (s `div` 2) ut ctx t
    return $ IfExpr e1 e2 e3

-------------------------------------------------------------------------------
-- Generating Tuple Expressions                                              --
-------------------------------------------------------------------------------

genTupleExpr :: Int -> UT -> Ctx -> Type -> Maybe (Gen Expression)
genTupleExpr s ut ctx (TypeTuple tt) = 
    let es = mapM (genExpression (s `div` 2) ut ctx) tt
      in Just $ TupleExpr <$> es    
genTupleExpr _ _ _ _                = Nothing

-------------------------------------------------------------------------------
-- Generating List Expressions                                               --
-------------------------------------------------------------------------------

genExprList :: Int -> UT -> Ctx -> Type -> Gen [Expression]
genExprList s ut ctx t = do
    mx <- chooseInt (1, maxListSize)
    vectorOf mx (genExpression (s `div` 2) ut ctx t)    

genListExpr :: Int -> UT -> Ctx -> Type -> Maybe (Gen Expression)
genListExpr s ut ctx (TypeList t) = Just $ ListExpr <$> genExprList s ut ctx t
genListExpr _ _ _ _               = Nothing

-------------------------------------------------------------------------------
-- Generating Constructor Expressions                                        --
-------------------------------------------------------------------------------

genConstructorExpr :: Int -> UT -> Ctx -> Constructor -> Gen Expression
genConstructorExpr s ut ctx (Constructor n ts) = 
    let es = mapM (genExpression (s `div` 2) ut ctx) ts
      in ConstructorExpr n <$> es

genAlgebraicExpr :: Int -> UT -> Ctx -> Type -> Maybe (Gen Expression)
genAlgebraicExpr s ut ctx (TypeAlgebraic n ts) = 
    Just (genConstructorExpr s ut ctx =<< elements ts)
genAlgebraicExpr _ _ _ _                       = Nothing

-------------------------------------------------------------------------------
-- Generating Expressions                                                    --
-------------------------------------------------------------------------------

genFinalExpression :: UT -> Ctx -> Type -> Gen Expression
genFinalExpression ut ctx t@(TypeTuple ts) = 
    let es = catMaybes [genTupleExpr 0 ut ctx t, 
                        genVarExpr ctx t]
      in do join (elements es)
genFinalExpression ut ctx t@(TypeList t') = 
    let es = catMaybes [genListExpr 0 ut ctx t, 
                        genVarExpr ctx t]
      in do join (elements es)
genFinalExpression ut ctx t@(TypePrim t') = 
    let es = catMaybes [genLiteralExpr t, 
                        genVarExpr ctx t]
      in do join (elements es)
genFinalExpression ut ctx t@(TypeFunction t1 t2) = 
    let es = catMaybes [genLambdaExpr 0 ut ctx t, 
                        genVarExpr ctx t]
      in do join (elements es)
genFinalExpression ut ctx t@(TypeAlgebraic n ts) = 
    let es = catMaybes [genAlgebraicExpr 0 ut ctx t,
                        genVarExpr ctx t]
      in do join (elements es)
-- genFinalExpression ut ctx t = error $ "genFinalExpression for (" ++ show t ++ ") not implemented."

genRecursiveExpression :: Int -> UT -> Ctx -> Type -> Gen Expression
genRecursiveExpression s ut ctx t = 
    let es1 = catMaybes [genLiteralExpr t, 
                         genVarExpr ctx t, 
                         genLambdaExpr s ut ctx t, 
                         genTupleExpr (s `div` 2) ut ctx t,
                         genListExpr (s `div` 2) ut ctx t,
                         genAlgebraicExpr (s `div` 2) ut ctx t]
        es2 = [genAppExpr s ut ctx t, 
               genLetExpr s ut ctx t, 
               genCaseExpr s ut ctx t, 
               genIfExpr s ut ctx t]         
      in do join (elements (es1 ++ es2))

genExpression :: Int -> UT -> Ctx -> Type -> Gen Expression
genExpression s ut ctx t | s > 0 = genRecursiveExpression (s `div` 2) ut ctx t
                         | otherwise = genFinalExpression ut ctx t


-- instance Arbitrary Expression where
--     arbitrary = sized (\s -> do genExpression s [] =<< genType s)