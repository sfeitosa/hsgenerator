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
    ps <- mapM genPattern (getConstructorTypes c)
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

genTuplePattern :: [Type] -> Gen Pattern
genTuplePattern ts = do
    ps <- mapM genPattern ts
    return $ TuplePattern ps

genListPattern :: Type -> Gen Pattern 
genListPattern t = frequency [(10, return EmptyListPattern), 
                              (2, ConsPattern <$> genPattern t <*> genPattern (TypeList t))]

genPattern :: Type -> Gen Pattern
genPattern (TypePrim t) = frequency [(10, genVarPattern), 
                                     (5, genWildcardPattern), 
                                     (1, genLiteralPattern t)]
genPattern (TypeTuple ts) = frequency [(10, genVarPattern), 
                                       (5, genWildcardPattern), 
                                       (1, genTuplePattern ts)]
genPattern (TypeList t) = frequency [(10, genVarPattern), 
                                     (5, genWildcardPattern), 
                                     (1, genListPattern t)]
genPattern (TypeAlgebraic ts) = frequency [(1, genVarPattern), 
                                           (1, genWildcardPattern), 
                                           (1, genConstructorPattern ts)]
genPattern t = frequency [(10, genVarPattern), 
                          (1, genWildcardPattern)]