module HSGenPattern where 

import Test.QuickCheck
import Consts
import Utils
import HSSyntax

-------------------------------------------------------------------------------
-- Generating Patterns                                                       --
-------------------------------------------------------------------------------

genVarPattern :: Gen Pattern
genVarPattern = do
    n <- elements varNames -- @TODO: get and remove var name
    return $ VarPattern n

genWildcardPattern :: Gen Pattern
genWildcardPattern = do return WildcardPattern

genConstructorPattern :: [Constructor] -> Gen Pattern
genConstructorPattern cs = do
    c <- elements cs
    ps <- mapM (genPattern 1) (getConstructorTypes c)
    return $ ConstructorPattern (getConstructorName c) ps

genLiteralPattern :: PrimType -> Gen Pattern
genLiteralPattern IntType = do
    n <- chooseInt (minLitBound, maxLitBound)
    return $ LiteralPattern (IntLiteral n)
genLiteralPattern IntegerType = do LiteralPattern . IntegerLiteral <$> arbitrary
genLiteralPattern FloatType = do LiteralPattern . FloatLiteral <$> arbitrary
genLiteralPattern DoubleType = do LiteralPattern . DoubleLiteral <$> arbitrary
genLiteralPattern StringType = do
    n <- getPrintableString <$> arbitrary
    return $ LiteralPattern (StringLiteral n)
genLiteralPattern BoolType = do LiteralPattern . BoolLiteral <$> arbitrary
genLiteralPattern CharType = do LiteralPattern . CharLiteral <$> arbitraryPrintableChar

genTuplePattern :: [Type] -> Gen Pattern
genTuplePattern ts = do
    ps <- mapM (genPattern 1) ts
    return $ TuplePattern ps

genListPattern :: Type -> Gen Pattern 
genListPattern t = frequency [(10, return EmptyListPattern), 
                              (2, ConsPattern <$> genPattern 1 t <*> genPattern 1 (TypeList t))]

genPattern :: Int -> Type -> Gen Pattern
genPattern _ t@(TypeFunction t1 t2) = genGeneralPattern t -- @TODO: verify if this is correct
genPattern 0 t = genGeneralPattern t
genPattern n t = genSpecificPattern t

genGeneralPattern :: Type -> Gen Pattern
genGeneralPattern t = frequency [(10, genVarPattern), 
                                 (5, genWildcardPattern)]

genSpecificPattern :: Type -> Gen Pattern
genSpecificPattern (TypePrim t) = frequency [(1, genLiteralPattern t)]
genSpecificPattern (TypeTuple ts) = frequency [(1, genTuplePattern ts)]
genSpecificPattern (TypeList t) = frequency [(1, genListPattern t)]
genSpecificPattern (TypeAlgebraic ts) = frequency [(1, genConstructorPattern ts)]
genSpecificPattern t = error $ "genSpecificPattern for " ++ show t ++ " not implemented"

