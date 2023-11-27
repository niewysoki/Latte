module Latte.Analysis.Data where

import qualified Data.Map as M
import Latte.Grammar.Abs

data FunctionType = ITFun InternalType [InternalType] deriving (Eq, Show)

data InternalType
    = ITInt
    | ITStr
    | ITBool
    | ITVoid
    deriving (Eq, Show)

fromType :: Type -> InternalType
fromType (TInt _) = ITInt
fromType (TStr _) = ITStr
fromType (TBool _) = ITBool
fromType (TVoid _) = ITVoid
fromType _ = error "TODO"

funcTypePrintInt, funcTypePrintString, funcTypeError, funcTypeReadInt, funcTypeReadString :: FunctionType
funcTypePrintInt = ITFun ITVoid [ITInt]
funcTypePrintString = ITFun ITVoid [ITStr]
funcTypeError = ITFun ITVoid []
funcTypeReadInt = ITFun ITInt []
funcTypeReadString = ITFun ITStr []

type FunctionMap = M.Map Ident FunctionType
runtimeFuncTypes :: FunctionMap
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
funcNameMain :: Ident
funcNameMain = Ident "main"