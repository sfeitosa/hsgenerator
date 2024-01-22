module HSGenDeclaration where 

import Test.QuickCheck
import Consts
import HSSyntax
import HSGenType

-------------------------------------------------------------------------------
-- Generating Function declarations                                          --
-------------------------------------------------------------------------------

genVarPattern :: Gen Pattern
genVarPattern = do
    n <- elements varNames -- @TODO: get and remove var name
    return $ VarPattern n

genWildcardPattern :: Gen Pattern
genWildcardPattern = do return WildcardPattern

-- genConstructorPattern :: Gen Pattern
-- genConstructorPattern = do
--     n  <- elements constrNames -- @TODO: get existing constructor names
--     mx <- chooseInt (2, maxParamSize)
--     ps <- vectorOf mx genPattern
--     ConstructorPattern n <$> ps

genPattern :: Type -> Gen Pattern
genPattern t = frequency [(10, genVarPattern), 
                          (1, genWildcardPattern)] 
                          -- (2, genConstructorPattern)]

genFunctionDecl :: Gen Declaration
genFunctionDecl = do
    n <- elements funNames -- @TODO: get and remove fun name
    mx <- chooseInt (0, maxParamSize)
    ts <- vectorOf mx arbitrary
    ps <- mapM genPattern ts
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
