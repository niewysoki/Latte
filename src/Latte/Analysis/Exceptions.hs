{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Latte.Analysis.Exceptions where

import qualified Data.Kind as DK
import Data.List (intercalate)
import Latte.Analysis.Data (InternalType, runtimeFuncNames)
import Latte.Grammar.Abs (BNFC'Position, Ident (..))

data StaticException
    = SENoSuchFunction BNFC'Position Ident
    | SENoSuchVariable BNFC'Position Ident
    | SEVariableRedeclaration BNFC'Position Ident InternalType
    | SEVariableAssignmentTypeMismatch BNFC'Position Ident InternalType InternalType
    | SEExpressionExpectedEqualTypes BNFC'Position String InternalType InternalType
    | SEExpressionExpectedAllowedType BNFC'Position String [InternalType] InternalType
    | SEIsNotFunction BNFC'Position
    | SEArgumentNumberMismatch BNFC'Position Ident Int Int
    | SEArgumentTypeMismatch BNFC'Position Ident InternalType InternalType
    | SEIntLiteralOutOfBounds BNFC'Position Integer Integer Integer
    | SEExpectedExpressionOfType BNFC'Position InternalType InternalType
    | SEExpectedVariableOfType BNFC'Position InternalType InternalType
    | SEVoidVariableDeclaration BNFC'Position
    | SEVoidFunctionArgument BNFC'Position Ident Ident
    | SERepeatedFunctionArgumentName BNFC'Position Ident Ident
    | SEFunctionRedeclaration Ident
    | SENoMain
    | SENoReturn Ident
    deriving (Show)

throw :: forall (m :: DK.Type -> DK.Type) a. MonadFail m => StaticException -> m a
throw (SENoSuchFunction pos (Ident name)) = fail $ "Compilation error: No function found for name: " ++ name ++ ", at " ++ showPos pos
throw (SENoSuchVariable pos (Ident name)) = fail $ "Compilation error: No variable found for name: " ++ name ++ ", at " ++ showPos pos
throw (SEVariableRedeclaration pos ident originalType) = fail $ "Compilation error: Redeclaration of " ++ showVar originalType ident ++ ", at " ++ showPos pos
throw (SEVariableAssignmentTypeMismatch pos ident exprType varType) = fail $ "Compilation error: Assignment of a value of type " ++ show exprType ++ " to " ++ showVar varType ident ++ ", at " ++ showPos pos
throw (SEExpressionExpectedEqualTypes pos exprStr leftType rightType) = fail $ "Compilation error: Expected equal types, but got left operand type: " ++ show leftType ++ " right operand type: " ++ show rightType ++ " in expression " ++ exprStr ++ ", at " ++ showPos pos
throw (SEExpressionExpectedAllowedType pos exprStr allowedTypes t) = fail $ "Compilation error: Expected one of " ++ intercalate ", " (map show allowedTypes) ++ " types, but instead got " ++ show t ++ " in expression " ++ exprStr ++ ", at " ++ showPos pos
throw (SEIsNotFunction pos) = fail $ "Compilation error: Left operand is not a function at " ++ showPos pos
throw (SEArgumentNumberMismatch pos (Ident name) expectedArgNum providedArgNum) = fail $ "Compilation error: Function " ++ name ++ " takes " ++ show expectedArgNum ++ " arguments, but " ++ show providedArgNum ++ " were provided, at " ++ showPos pos
throw (SEArgumentTypeMismatch pos (Ident name) expectedArgType providedArgType) = fail $ "Compilation error: Function " ++ name ++ " expected an argument of type " ++ show expectedArgType ++ ", but " ++ show providedArgType ++ " was provided, at " ++ showPos pos
throw (SEIntLiteralOutOfBounds pos lit lo hi) = fail $ "Compilation error: integer value " ++ show lit ++ " is outside of 32-bit bounds [" ++ show lo ++ "," ++ show hi ++ "], at " ++ showPos pos
throw (SEExpectedExpressionOfType pos expectedType providedType) = fail $ "Compilation error: Expected value of type " ++ show expectedType ++ ", but got " ++ show providedType ++ ", at " ++ showPos pos
throw (SEExpectedVariableOfType pos expectedType providedType) = fail $ "Compilation error: Expected variable of type " ++ show expectedType ++ ", but got variable of type " ++ show providedType ++ " in incrementation / decrementation statement at " ++ showPos pos
throw (SEVoidVariableDeclaration pos) = fail $ "Compilation error: Declaration of void variable type at " ++ showPos pos
throw (SEVoidFunctionArgument pos (Ident funName) (Ident argName)) = fail $ "Compilation error: Function argument " ++ argName ++ " declared with illegal type void, in function " ++ funName ++ ", at " ++ showPos pos
throw (SERepeatedFunctionArgumentName pos (Ident funName) (Ident argName)) = fail $ "Compilation error: Function argument " ++ argName ++ " declared more than once in function " ++ funName ++ ", at " ++ showPos pos
throw (SEFunctionRedeclaration (Ident name)) = fail $ "Compilation error: Function " ++ name ++ " declared more than once! Reserved function names: " ++ intercalate ", " runtimeFuncNames
throw SENoMain = fail "Compilation error: No int main() function"
throw (SENoReturn (Ident name)) = fail $ "Compilation error: In function " ++ name ++ " not all execution paths terminate in return"

showPos :: BNFC'Position -> String
showPos (Just (x, y)) = "row: " ++ show x ++ ", col: " ++ show y
showPos Nothing = "unknown position"

showVar :: InternalType -> Ident -> String
showVar t (Ident name) = "variable " ++ name ++ " of type " ++ show t