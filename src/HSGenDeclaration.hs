module HSGenDeclaration where 

import Test.QuickCheck
import Consts
import HSSyntax
import HSGenPattern
import HSGenType

-------------------------------------------------------------------------------
-- Generating Function declarations                                          --
-------------------------------------------------------------------------------

genFunctionDecl :: Gen Declaration
genFunctionDecl = do
    n <- elements funNames -- @TODO: get and remove fun name
    mx <- chooseInt (0, maxParamSize)
    ts <- vectorOf mx arbitrary
    ps <- mapM (genPattern 1) ts
    return $ FunctionDecl n ts ps (LiteralExpr (StringLiteral "@TODO: generate expressions"))

-------------------------------------------------------------------------------
-- Generating ADT declarations                                               --
-------------------------------------------------------------------------------

genConstructor :: Gen Constructor
genConstructor = do
    n  <- elements dataNames -- @TODO: get and remove data names
    mx <- chooseInt (0, maxParamSize)
    ts <- vectorOf mx arbitrary
    return $ Constructor n ts

genDataDecl :: Gen Declaration
genDataDecl = do
    n <- elements dataNames -- @TODO: get and remove data name
    mx <- chooseInt (1, maxConstrSize)
    cs <- vectorOf mx genConstructor
    return $ DataDecl n cs

-------------------------------------------------------------------------------
-- Generating Type declarations                                              --
-------------------------------------------------------------------------------

genTypeDecl :: Gen Declaration
genTypeDecl = do
    n <- elements dataNames -- @TODO: get and remove data name
    TypeDecl n <$> arbitrary
