module Utils where

import Test.QuickCheck
import HSSyntax

getPatternVars :: Type -> Pattern -> [(String, Type)]
getPatternVars t (VarPattern v) = [(v, t)]
getPatternVars t WildcardPattern = []
getPatternVars t (ConstructorPattern _ ps) = [] -- @TODO: implement this