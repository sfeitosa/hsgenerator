module Utils where

import Test.QuickCheck
import HSSyntax

getPatternVars :: Type -> Pattern -> [(String, Type)]
getPatternVars t (VarPattern v) = [(v, t)]
getPatternVars t WildcardPattern = []
getPatternVars t (ConstructorPattern _ ps) = [] -- @TODO: implement this
getPatternVars _ _ = []

getLambdaParamTypes :: Expression -> [Type]
getLambdaParamTypes (LambdaExpr ts _ _) = ts

getConstructorName :: Constructor -> String
getConstructorName (Constructor n _) = n

getConstructorTypes :: Constructor -> [Type]
getConstructorTypes (Constructor _ ts) = ts
