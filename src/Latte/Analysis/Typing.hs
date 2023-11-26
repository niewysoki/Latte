{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Latte.Analysis.Typing where

import Control.Monad (unless, void, when)
import Control.Monad.State (StateT (runStateT), evalStateT, get, gets, lift, modify)
import Data.Int (Int32)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Latte.Analysis.Data
import Latte.Grammar.Abs
import Latte.Grammar.ErrM (Err, pattern Bad, pattern Ok)

data Env = Env
    { _outer_vars :: M.Map Ident InternalType
    , _inner_vars :: M.Map Ident InternalType
    , _funcs :: M.Map Ident FunctionType
    , _expected_return_type :: Maybe InternalType
    }

startEnv :: Env
startEnv = Env{_outer_vars = M.empty, _inner_vars = M.empty, _funcs = runtimeFuncTypes, _expected_return_type = Nothing}

type TypingState = StateT Env Err

type family TypingResult a where
    TypingResult Expr = InternalType
    TypingResult Stmt = ()
    TypingResult StmtBlock = ()
    TypingResult Type = InternalType

class Typed a where
    evalType :: a -> TypingState (TypingResult a)

instance Typed StmtBlock where
    evalType (StmtBlock _ stmts) = preservingEnv (mapM_ evalType stmts)

instance Typed Stmt where
    evalType (SEmpty _) = return ()
    evalType (SBlock _ block) = evalType block
    evalType (SDecl _ declType declarations) = do
        let mbType = fromType declType
        case mbType of
            Bad msg -> fail msg
            Ok declType' -> mapM_ (declare declType') declarations
    evalType (SAss p ident expr) = do
        exprType <- evalType expr
        varType <- getVarType p ident
        unless (exprType == varType) $ fail $ "TODO"
    evalType (SIncr p ident) = do
        varType <- getVarType p ident
        unless (varType == ITInt) $ fail $ "TODO"
    evalType (SDecr p ident) = do
        varType <- getVarType p ident
        unless (varType == ITInt) $ fail $ "TODO"
    evalType (SRet p expr) = do
        exprType <- evalType expr
        (Just expected) <- gets _expected_return_type
        unless (exprType == expected) $ fail "TODO"
    evalType (SVRet p) = do
        (Just expected) <- gets _expected_return_type
        unless (expected == ITVoid) $ fail "TODO"
    evalType (SCond p expr stmt) = do
        exprType <- evalType expr
        unless (exprType == ITBool) $ fail "TODO"
        preservingEnv (evalType stmt)
    evalType (SCondElse p expr true false) = do
        exprType <- evalType expr
        unless (exprType == ITBool) $ fail "TODO"
        preservingEnv (evalType true)
        preservingEnv (evalType false)
    evalType (SWhile p expr stmt) = do
        exprType <- evalType expr
        unless (exprType == ITBool) $ fail "TODO"
        preservingEnv (evalType stmt)
    evalType (SExp _ expr) = void $ evalType expr

instance Typed Expr where
    evalType (EVar p ident) = getVarType p ident
    evalType (ELitInt p lit) = do
        let lo = toInteger $ minBound @Int32
            hi = toInteger $ maxBound @Int32
        unless (lit >= lo && lit <= hi) $ fail $ "TODO BAD LITERAL INT " ++ show p ++ show lo ++ show hi
        return ITInt
    evalType (ELitTrue _) = return ITBool
    evalType (ELitFalse _) = return ITBool
    evalType (EString _ _) = return ITStr
    evalType (EApp p (EVar p' ident) argExprs) = do
        argTypes <- mapM evalType argExprs
        ITFun retT expectedArgTypes <- getFuncType p' ident
        unless (length argTypes == length expectedArgTypes) $ fail $ "TODO"
        unless (argTypes == expectedArgTypes) $ fail $ "TODO"
        return retT
    evalType (EApp p exp argExprs) = fail "TODO"
    evalType (ENeg p x) = do
        t <- expectEqualTypes [x] "TODO"
        expectAllowedType [ITInt] t "TODO"
        return t
    evalType (ENot p x) = do
        t <- expectEqualTypes [x] "TODO"
        expectAllowedType [ITBool] t "TODO"
        return t
    evalType (EAnd p x y) = do
        t <- expectEqualTypes [x, y] "TODO"
        expectAllowedType [ITBool] t "TODO"
        return t
    evalType (EOr p x y) = do
        t <- expectEqualTypes [x, y] "TODO"
        expectAllowedType [ITBool] t "TODO"
        return t
    evalType (EMul p x op y) = do
        t <- expectEqualTypes [x, y] "TODO"
        expectAllowedType [ITInt] t "TODO"
        return t
    evalType (EAdd p x op@(Minus _) y) = do
        t <- expectEqualTypes [x, y] "TODO"
        expectAllowedType [ITInt] t "TODO"
        return t
    evalType (EAdd p x op@(Plus _) y) = do
        t <- expectEqualTypes [x, y] "TODO"
        expectAllowedType [ITInt, ITStr] t "TODO"
        return t
    evalType (ERel p x op@(EQU _) y) = expectEqualTypes [x, y] "TODO"
    evalType (ERel p x op@(NE _) y) = expectEqualTypes [x, y] "TODO"
    evalType (ERel p x op y) = do
        t <- expectEqualTypes [x, y] "TODO"
        expectAllowedType [ITInt, ITStr] t "TODO"
        return t

expectAllowedType :: Show a => [InternalType] -> InternalType -> a -> TypingState ()
expectAllowedType allowedTypes t errSource = do
    unless (t `elem` allowedTypes) $ fail "TODO"

expectEqualTypes :: Show a => [Expr] -> a -> TypingState InternalType
expectEqualTypes expressions errSource = do
    when (null expressions) $ fail "TODO"
    types <- mapM evalType expressions
    let opType = head types
    unless (all (== opType) types) $ fail "TODO"
    return opType

declare :: InternalType -> Declaration -> TypingState ()
declare declType (DeclNoInit p ident) = do
    mbInner <- gets (M.lookup ident . _inner_vars)
    case mbInner of
        Just vt -> fail "TODO"
        Nothing -> modify $ \env -> env{_inner_vars = M.insert ident declType (_inner_vars env)}
declare declType (DeclInit p ident expr) = do
    mbInner <- gets (M.lookup ident . _inner_vars)
    case mbInner of
        Just vt -> fail "TODO"
        Nothing -> do
            exprType <- evalType expr
            unless (exprType == declType) $ fail $ "TODO"
            modify $ \env -> env{_inner_vars = M.insert ident declType (_inner_vars env)}

getVarType :: BNFC'Position -> Ident -> TypingState InternalType
getVarType p ident = do
    mbInner <- gets (M.lookup ident . _inner_vars)
    mbOuter <- gets (M.lookup ident . _outer_vars)
    case mergeMaybe mbInner mbOuter of
        Nothing -> fail "TODO"
        Just vt -> return vt
  where
    mergeMaybe :: Maybe a -> Maybe a -> Maybe a
    mergeMaybe (Just x) _ = Just x
    mergeMaybe Nothing x = x

getFuncType :: BNFC'Position -> Ident -> TypingState FunctionType
getFuncType p ident = do
    mbFuncType <- gets (M.lookup ident . _funcs)
    case mbFuncType of
        Nothing -> fail "TODO MESSAGE"
        Just ft -> return ft

preservingEnv :: TypingState a -> TypingState a
preservingEnv comp = do
    env <- get
    modify $ \e -> e{_outer_vars = M.union (_inner_vars e) (_outer_vars e), _inner_vars = M.empty}
    ret <- comp
    modify $ const env
    return ret