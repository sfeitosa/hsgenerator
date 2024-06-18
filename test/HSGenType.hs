module HSGenType where

import Test.QuickCheck
import Consts
import HSSyntax

data TType = AllTypes | MatchingTypes

genTypePrim :: Gen Type
genTypePrim = do
    t <- elements [IntType, IntegerType, FloatType, DoubleType, StringType, BoolType, CharType]
    return $ TypePrim t

genTypeTuple :: Int -> TType -> UT -> Gen Type
genTypeTuple s w ut = do
    mx <- chooseInt (2, maxTupleSize)
    ts <- vectorOf mx (genType (s `div` 2) w ut)
    return $ TypeTuple ts

genTypeList :: Int -> TType -> UT -> Gen Type
genTypeList s w ut = do
    t <- genType (s `div` 2) w ut
    return $ TypeList t

genTypeFunction :: Int -> TType -> UT -> Gen Type
genTypeFunction s w ut = do
    mx <- chooseInt (1, maxParamSize)
    ts <- vectorOf mx (genType (s `div` 2) w ut)
    tr <- genType (s `div` 2) w ut
    return $ TypeFunction ts tr

genType :: Int -> TType -> UT -> Gen Type
genType s w@AllTypes ut = genAllType s w ut
genType s w@MatchingTypes ut = genMatchingType s w ut

genAllType :: Int -> TType -> UT -> Gen Type
genAllType s w [] = frequency [(50, genTypePrim), 
                          (5, genTypeTuple (s `div` 2) w []), 
                          (10, genTypeList (s `div` 2) w []),
                          (10, genTypeFunction (s `div` 2) w [])]
genAllType s w ut | s > 0     = frequency [(20, genTypePrim), 
                                   (5, genTypeTuple (s `div` 2) w ut), 
                                   (10, genTypeList (s `div` 2) w ut), 
                                   (10, genTypeFunction (s `div` 2) w ut),
                                   (10, elements ut)]
                  | otherwise = genTypePrim

genMatchingType :: Int -> TType -> UT -> Gen Type 
genMatchingType s w [] = frequency [(50, genTypePrim), 
                                  (5, genTypeTuple (s `div` 2) w []), 
                                  (10, genTypeList (s `div` 2) w [])]
genMatchingType s w ut | s > 0     = frequency [(20, genTypePrim), 
                                        (5, genTypeTuple (s `div` 2) w ut), 
                                        (10, genTypeList (s `div` 2) w ut), 
                                        (10, elements ut)]
                       | otherwise = genTypePrim