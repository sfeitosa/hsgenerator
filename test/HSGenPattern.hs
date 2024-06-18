module HSGenPattern where 

import Test.QuickCheck
import Consts
import Utils
import HSSyntax

data TPattern = General | Specific

-------------------------------------------------------------------------------
-- Generating Patterns                                                       --
-------------------------------------------------------------------------------

genVarPattern :: Int -> Gen Pattern
genVarPattern s = do
    n <- getNextName s varNames -- @TODO: get and remove var name
    return $ VarPattern n

genWildcardPattern :: Gen Pattern
genWildcardPattern = do return WildcardPattern

genConstructorPattern :: Int -> [Constructor] -> Gen Pattern
genConstructorPattern s cs = do
    c <- elements cs
    ps <- mapM (genPattern s Specific) (getConstructorTypes c)
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

genTuplePattern :: Int -> [Type] -> Gen Pattern
genTuplePattern s ts = do
    ps <- mapM (genPattern s Specific) ts
    return $ TuplePattern ps

genListPattern :: Int -> Type -> Gen Pattern 
genListPattern s t = frequency [(10, return EmptyListPattern), 
                                (2, ConsPattern <$> genPattern s Specific t <*> genPattern s Specific (TypeList t))]

genPattern :: Int -> TPattern -> Type -> Gen Pattern
genPattern s _ t@(TypeFunction t1 t2) = genGeneralPattern s t -- @TODO: verify if this is correct
genPattern s General t = genGeneralPattern s t
genPattern s Specific t = genSpecificPattern s t

genGeneralPattern :: Int -> Type -> Gen Pattern
genGeneralPattern s t = frequency [(10, genVarPattern s), 
                                   (5, genWildcardPattern)]

genSpecificPattern :: Int -> Type -> Gen Pattern
genSpecificPattern s (TypePrim t) = genGeneralPattern s (TypePrim t)
genSpecificPattern s (TypeTuple ts) = frequency [(1, genTuplePattern s ts), (1, genGeneralPattern s (TypeTuple ts))]
genSpecificPattern s (TypeList t) = frequency [(1, genListPattern s t), (1, genGeneralPattern s (TypeList t))]
genSpecificPattern s (TypeAlgebraic n ts) = frequency [(1, genConstructorPattern s ts), (1, genGeneralPattern s (TypeAlgebraic n ts))]
genSpecificPattern s t = error $ "genSpecificPattern for " ++ show t ++ " not implemented"

