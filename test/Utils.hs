module Utils where

import Test.QuickCheck
import HSSyntax

getNextName :: Int -> [String] -> Gen String
getNextName nc l = do n <- elements l 
                      r <- chooseInt (0, 10000)
                      return (n ++ show (nc + r))

getPatternVars :: Type -> Pattern -> [(String, Type)]
getPatternVars t (VarPattern v) = [(v, t)]
getPatternVars t WildcardPattern = []
getPatternVars t (ConstructorPattern _ ps) = [] -- @TODO: implement this
getPatternVars _ _ = []

getListPatternVars :: [Type] -> [Pattern] -> [(String, Type)]
getListPatternVars [] [] = []
getListPatternVars (t:ts) (p:ps) = getPatternVars t p ++ getListPatternVars ts ps

getLambdaParamTypes :: Expression -> [Type]
getLambdaParamTypes (LambdaExpr ts _ _) = ts

getConstructorName :: Constructor -> String
getConstructorName (Constructor n _) = n

getConstructorTypes :: Constructor -> [Type]
getConstructorTypes (Constructor _ ts) = ts

getFunctionInfo :: Declaration -> (String, Type)
getFunctionInfo (FunctionDecl n ts _ rt _) = (n, TypeFunction ts rt)

getADTInfo :: Declaration -> Type
getADTInfo (DataDecl n cs) = TypeAlgebraic n cs