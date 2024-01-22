module HSGenType where

import Test.QuickCheck
import Consts
import HSSyntax

genTypePrim :: Gen Type
genTypePrim = do
    t <- elements [IntType, IntegerType, FloatType, DoubleType, StringType, BoolType, CharType]
    return $ TypePrim t

genTypeTuple :: Int -> Gen Type
genTypeTuple s = do
    mx <- chooseInt (2, maxTupleSize)
    ts <- vectorOf mx (genType (s `div` 2))
    return $ TypeTuple ts

genTypeList :: Int -> Gen Type
genTypeList s = do
    t <- genType (s `div` 2)
    return $ TypeList t

genTypeFunction :: Int -> Gen Type
genTypeFunction s = do
    t1 <- genType (s `div` 2)
    t2 <- genType (s `div` 2)
    return $ TypeFunction t1 t2

genType :: Int -> Gen Type
genType s | s > 0     = frequency [(100, genTypePrim), 
                                   (5, genTypeTuple s'), 
                                   (10, genTypeList s'), 
                                   (10, genTypeFunction s')]
          | otherwise = genTypePrim
    where s' = s `div` 2

instance Arbitrary Type where
    arbitrary = sized genType