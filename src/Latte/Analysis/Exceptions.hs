{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Latte.Analysis.Exceptions where

import qualified Data.Kind as DK
import Latte.Analysis.Data (InternalType)
import Latte.Grammar.Abs

data StaticException
    = SENoSuchFunction BNFC'Position Ident
    | SENoSuchVariable BNFC'Position Ident
    | SEVariableRedeclaration BNFC'Position Ident InternalType InternalType
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

showPos :: BNFC'Position -> String
showPos (Just (x, y)) = "col: " ++ show x ++ ", row: " ++ show y
showPos Nothing = "unknown position"

throw :: forall (m :: DK.Type -> DK.Type) a. MonadFail m => StaticException -> m a
throw (SENoSuchFunction pos (Ident name)) = fail $ "Compilation error: No function found for name: " ++ name ++ ", at " ++ showPos pos
throw (SENoSuchVariable pos (Ident name)) = fail $ "Compilation error: No variable found for name: " ++ name ++ ", at " ++ showPos pos
throw exc = fail $ show exc