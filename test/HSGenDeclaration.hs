module HSGenDeclaration where 

import Test.QuickCheck
import Consts
import Utils
import HSSyntax
import HSGenPattern
import HSGenType
import HSGenExpression

-------------------------------------------------------------------------------
-- Generating Function declarations                                          --
-------------------------------------------------------------------------------

genFunctionDecl :: Int -> UT -> Ctx -> Gen Declaration
genFunctionDecl s ut ctx = do
    n <- getNextName (s + length ut + length ctx) funNames -- @TODO: get and remove fun name
    mx <- chooseInt (0, maxParamSize)
    ts <- vectorOf mx (genType s AllTypes ut)
    ps <- mapM (genPattern s General) ts -- @TODO: generate more than one equation
    rt <- genType s AllTypes ut
    b  <- sized (\s -> genExpression 
                         (s `div` 2) ut
                         (concatMap (uncurry getPatternVars) (zip ts ps) ++ ctx) rt)
    return $ FunctionDecl n ts ps rt b

-------------------------------------------------------------------------------
-- Generating ADT declarations                                               --
-------------------------------------------------------------------------------

genConstructor :: Int -> UT -> Ctx -> Gen Constructor
genConstructor s ut ctx = do
    n  <- getNextName (s + length ut + length ctx) dataNames -- @TODO: get and remove data names
    mx <- chooseInt (0, maxParamSize)
    ts <- vectorOf mx (genType s MatchingTypes ut)
    return $ Constructor n ts

genDataDecl :: Int -> UT -> Ctx -> Gen Declaration
genDataDecl s ut ctx = do
    n <- getNextName (s + length ut + length ctx) dataNames -- @TODO: get and remove data name
    mx <- chooseInt (1, maxConstrSize)
    cs <- vectorOf mx (genConstructor s ut ctx) -- @TODO: allow recursive ADTs
    return $ DataDecl n cs

-------------------------------------------------------------------------------
-- Generating Type declarations                                              --
-------------------------------------------------------------------------------

genTypeDecl :: Int -> UT -> Gen Declaration
genTypeDecl s ut = do
    n <- getNextName (s + length ut) dataNames -- @TODO: get and remove data name
    t <- genType s AllTypes ut
    return $ TypeDecl n t
