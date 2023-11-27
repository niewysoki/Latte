module Latte.Analysis.Data where

import Data.List (intercalate)
import qualified Data.Map as M
import Latte.Grammar.Abs

data FunctionType = ITFun InternalType [InternalType] deriving (Eq)

instance Show FunctionType where
    show (ITFun rt argts) = show rt ++ "(" ++ intercalate "," (map show argts) ++ ")"

data InternalType
    = ITInt
    | ITStr
    | ITBool
    | ITVoid
    deriving (Eq)

instance Show InternalType where
    show ITInt = "int"
    show ITStr = "string"
    show ITBool = "bool"
    show ITVoid = "void"

fromType :: Type -> InternalType
fromType (TInt _) = ITInt
fromType (TStr _) = ITStr
fromType (TBool _) = ITBool
fromType (TVoid _) = ITVoid
fromType _ = error "Internal compiler error, contact the maintainer"

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

runtimeFuncNames :: [String]
runtimeFuncNames = map ((\(Ident name) -> name) . fst) $ M.toList runtimeFuncTypes

funcTypeMain :: FunctionType
funcTypeMain = ITFun ITInt []
funcNameMain :: Ident
funcNameMain = Ident "main"