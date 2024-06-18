module HSGenModule where

import Test.QuickCheck
import Consts
import Utils
import HSSyntax
import HSGenType
import HSGenExpression
import HSGenDeclaration

genADTs :: Int -> Int -> UT -> Ctx -> Gen (UT, [Declaration])
genADTs s 0 ut ctx = return (ut, [])
genADTs s n ut ctx = do
    d  <- genDataDecl s ut ctx
    (_, ds) <- genADTs (s `div` 2) (n-1) (getADTInfo d : ut) ctx
    return (getADTInfo d : ut, d : ds)

genFunctions :: Int -> Int -> UT -> Ctx -> Gen (Ctx, [Declaration])
genFunctions s 0 ut ctx = return (ctx, [])
genFunctions s n ut ctx = do
    f  <- genFunctionDecl s ut ctx
    (_, fs) <- genFunctions (s `div` 2) (n-1) ut (getFunctionInfo f : ctx)
    return (getFunctionInfo f : ctx, f : fs)


genMainFunction :: Int -> UT -> Ctx -> Gen Declaration
genMainFunction s ut ctx = do
    et <- genType s MatchingTypes ut
    e  <- genExpression (s `div` 2) ut ctx et
    return $ FunctionDecl "main" [] [] (TypeAlgebraic "IO ()" []) (AppExpr (FunExpr "print $ ") [e])

genDeclarations :: Int -> UT -> Ctx -> Gen [Declaration]
genDeclarations s ut ctx = do 
    nd   <- chooseInt (0, maxDeclSize)
    nf   <- chooseInt (0, maxDeclSize)
    (ut', adts)  <- genADTs s nd ut ctx
    (ctx', funs) <- genFunctions s nf ut' ctx
    mfun <- genMainFunction s ut' ctx'
    return $ adts ++ funs ++ [mfun]


genModule :: Gen Module
genModule = do Module "Main" [] <$> sized (\s -> genDeclarations s [] [])