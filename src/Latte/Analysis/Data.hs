module Latte.Analysis.Data where

import qualified Data.Map as M
import Latte.Grammar.Abs
import Latte.Grammar.ErrM

data FunctionType = ITFun InternalType [InternalType]

data InternalType
    = ITInt
    | ITStr
    | ITBool
    | ITVoid
    deriving (Eq, Show)

fromType :: Type -> Err InternalType
fromType (TInt _) = return ITInt
fromType (TStr _) = return ITStr
fromType (TBool _) = return ITBool
fromType (TVoid _) = return ITVoid
fromType _ = undefined

funcTypePrintInt, funcTypePrintString, funcTypeError, funcTypeReadInt, funcTypeReadString :: FunctionType
funcTypePrintInt = ITFun ITVoid [ITInt]
funcTypePrintString = ITFun ITVoid [ITStr]
funcTypeError = ITFun ITVoid []
funcTypeReadInt = ITFun ITInt []
funcTypeReadString = ITFun ITStr []

runtimeFuncTypes :: M.Map Ident FunctionType
runtimeFuncTypes =
    M.fromList
        [ (Ident "printInt", funcTypePrintInt)
        , (Ident "printString", funcTypePrintString)
        , (Ident "error", funcTypeError)
        , (Ident "readInt", funcTypeReadInt)
        , (Ident "readString", funcTypeReadString)
        ]

funcTypeMain :: FunctionType
funcTypeMain = ITFun ITInt []