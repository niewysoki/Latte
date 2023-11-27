{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Latte.Analysis.Typing where

import Control.Monad (unless, void, when)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.State (StateT (runStateT), get, gets, modify)
import Data.Int (Int32)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import Latte.Analysis.Data (
    FunctionMap,
    FunctionType (..),
    InternalType (..),
    fromType,
    funcNameMain,
    funcTypeMain,
    runtimeFuncTypes,
 )
import Latte.Analysis.Exceptions (StaticException (..), throw)
import Latte.Grammar.Abs (
    BNFC'Position,
    Declaration,
    Declaration' (DeclInit, DeclNoInit),
    Expr,
    Expr' (..),
    FunctionArg' (FArg),
    HasPosition (hasPosition),
    Ident,
    OpAdd' (Minus, Plus),
    OpRel' (EQU, NE),
    Program,
    Program' (Program),
    Stmt,
    Stmt' (..),
    StmtBlock,
    StmtBlock' (StmtBlock),
    TopDef,
    TopDef' (TDFunction),
 )
import Latte.Grammar.ErrM (Err, pattern Bad, pattern Ok)
import Latte.Grammar.Print (Print (..), render)

checkTypes :: Program -> Err ()
checkTypes prog = runReaderT (evalType prog) runtimeFuncTypes

data Env = Env
    { _outerVars :: M.Map Ident InternalType
    , _innerVars :: M.Map Ident InternalType
    , _funcs :: M.Map Ident FunctionType
    }

type TypingState a = StateT Env Err a
type TypingReader a = ReaderT FunctionMap Err a

type family TypingResult a where
    TypingResult Expr = TypingState InternalType
    TypingResult Stmt = InternalType -> TypingState ()
    TypingResult StmtBlock = InternalType -> TypingState ()
    TypingResult TopDef = TypingReader ()
    TypingResult Program = TypingReader ()

class Typed a where
    evalType :: a -> TypingResult a

instance Typed Program where
    evalType (Program _ topDefs) = do
        runtimeFs <- ask
        let fds = map functionDeclaration $ filter isFunction topDefs
            runtimeNames = map fst $ M.toList runtimeFs
            names = map fst fds ++ runtimeNames
            mbFstRedeclaration = dup names
            hasMain = (funcNameMain, funcTypeMain) `elem` fds

        when (isJust mbFstRedeclaration) $ throw $ SEFunctionRedeclaration (fromJust mbFstRedeclaration)
        unless hasMain $ throw SENoMain

        local (M.union (M.fromList fds)) $ mapM_ evalType topDefs
      where
        functionDeclaration (TDFunction _ retT name args _) =
            let rt = fromType retT
                ats = map (\(FArg _ t _) -> fromType t) args
             in (name, ITFun rt ats)

        isFunction (TDFunction{}) = True

instance Typed TopDef where
    evalType (TDFunction p retT ident args block) = do
        let unpackedArgs = map (\(FArg _ t n) -> (n, fromType t)) args
            mbVoidArg = L.find ((== ITVoid) . snd) unpackedArgs
        when (isJust mbVoidArg) $ throw $ SEVoidFunctionArgument p ident (fst $ fromJust mbVoidArg)

        let argNames = map fst unpackedArgs
            mbRepeatedName = dup argNames
        when (isJust mbRepeatedName) $ throw $ SERepeatedFunctionArgumentName p ident (fromJust mbRepeatedName)

        funcs <- ask
        let rt = fromType retT
            initEnv =
                Env
                    { _innerVars = M.empty
                    , _outerVars = M.fromList unpackedArgs
                    , _funcs = funcs
                    }

        case runStateT (evalType block rt) initEnv of
            Bad msg -> fail msg
            Ok _ -> return ()

instance Typed StmtBlock where
    evalType (StmtBlock _ stmts) rt = preservingEnv (mapM_ (`evalType` rt) stmts)

instance Typed Stmt where
    evalType (SEmpty _) _ = return ()
    evalType (SBlock _ block) rt = evalType block rt
    evalType (SDecl p declType declarations) _ = do
        let dt = fromType declType
        when (dt == ITVoid) $ throw $ SEVoidVariableDeclaration p
        mapM_ (declare dt) declarations
    evalType (SAss p ident expr) _ = do
        et <- evalType expr
        vt <- getVarType p ident
        unless (et == vt) $ throw $ SEVariableAssignmentTypeMismatch p ident et vt
    evalType (SIncr p ident) _ = do
        vt <- getVarType p ident
        unless (vt == ITInt) $ throw $ SEExpectedVariableOfType p ITInt vt
    evalType (SDecr p ident) _ = do
        vt <- getVarType p ident
        unless (vt == ITInt) $ throw $ SEExpectedVariableOfType p ITInt vt
    evalType (SRet p expr) expected = do
        et <- evalType expr
        unless (et == expected) $ throw $ SEExpectedExpressionOfType p expected et
    evalType (SVRet _) ITVoid = return ()
    evalType (SVRet p) expected = throw $ SEExpectedExpressionOfType p expected ITVoid
    evalType (SCond p expr stmt) rt = do
        et <- evalType expr
        unless (et == ITBool) $ throw $ SEExpectedExpressionOfType p ITBool et
        preservingEnv (evalType stmt rt)
    evalType (SCondElse p expr true false) rt = do
        et <- evalType expr
        unless (et == ITBool) $ throw $ SEExpectedExpressionOfType p ITBool et
        preservingEnv (evalType true rt)
        preservingEnv (evalType false rt)
    evalType (SWhile p expr stmt) rt = do
        et <- evalType expr
        unless (et == ITBool) $ throw $ SEExpectedExpressionOfType p ITBool et
        preservingEnv (evalType stmt rt)
    evalType (SExp _ expr) _ = void $ evalType expr

instance Typed Expr where
    evalType (EVar p ident) = getVarType p ident
    evalType (ELitInt p lit) = do
        unless (lit >= lo && lit <= hi) $ throw $ SEIntLiteralOutOfBounds p lit lo hi
        return ITInt
      where
        lo = toInteger $ minBound @Int32
        hi = toInteger $ maxBound @Int32
    evalType (ELitTrue _) = return ITBool
    evalType (ELitFalse _) = return ITBool
    evalType (EString _ _) = return ITStr
    evalType (EApp p (EVar p' ident) argExprs) = do
        ITFun retT expectedArgTypes <- getFuncType p' ident
        argTypes <- mapM evalType argExprs
        let argNumFun = length expectedArgTypes
            argNumProvided = length argTypes
            equalArgNum = argNumFun == argNumProvided
        unless equalArgNum $ throw $ SEArgumentNumberMismatch p ident argNumFun argNumProvided
        let argPairs = zip expectedArgTypes argTypes
            mbFstDiff = L.find (uncurry (/=)) argPairs
        when (isJust mbFstDiff) $ throw $ uncurry (SEArgumentTypeMismatch p ident) (fromJust mbFstDiff)
        return retT
    evalType (EApp p _ _) = throw $ SEIsNotFunction p
    evalType (ENeg p x) = do
        t <- evalType x
        expectType [ITInt] t "-" p
    evalType (ENot p x) = do
        t <- evalType x
        expectType [ITBool] t "!" p
    evalType (EAnd p x y) = evalBinOp p "&&" [ITBool] x y
    evalType (EOr p x y) = evalBinOp p "||" [ITBool] x y
    evalType (EMul p x op y) =
        let exprStr = render $ prt 0 op
         in evalBinOp p exprStr [ITInt] x y
    evalType (EAdd _ x op@(Minus p) y) =
        let exprStr = render $ prt 0 op
         in evalBinOp p exprStr [ITInt] x y
    evalType (EAdd _ x op@(Plus p) y) =
        let exprStr = render $ prt 0 op
         in evalBinOp p exprStr [ITInt, ITStr] x y
    evalType (ERel _ x op@(EQU p) y) =
        let exprStr = render $ prt 0 op
         in evalBinOp p exprStr [ITInt, ITStr, ITBool] x y >> return ITBool
    evalType (ERel _ x op@(NE p) y) =
        let exprStr = render $ prt 0 op
         in evalBinOp p exprStr [ITInt, ITStr, ITBool] x y >> return ITBool
    evalType (ERel _ x op y) =
        let exprStr = render $ prt 0 op
         in evalBinOp (hasPosition op) exprStr [ITInt, ITStr] x y >> return ITBool

evalBinOp :: BNFC'Position -> String -> [InternalType] -> Expr -> Expr -> TypingState InternalType
evalBinOp p exprStr allowedTypes x y = do
    t <- expectEqualTypes x y exprStr p
    expectType allowedTypes t exprStr p

expectType :: [InternalType] -> InternalType -> String -> BNFC'Position -> TypingState InternalType
expectType allowedTypes t exprStr p = do
    unless (t `elem` allowedTypes) $ throw $ SEExpressionExpectedAllowedType p exprStr allowedTypes t
    return t

expectEqualTypes :: Expr -> Expr -> String -> BNFC'Position -> TypingState InternalType
expectEqualTypes x y exprStr p = do
    tx <- evalType x
    ty <- evalType y
    unless (tx == ty) $ throw $ SEExpressionExpectedEqualTypes p exprStr tx ty
    return tx

declare :: InternalType -> Declaration -> TypingState ()
declare declType (DeclNoInit p ident) = do
    mbInner <- gets (M.lookup ident . _innerVars)
    case mbInner of
        Just vt -> throw $ SEVariableRedeclaration p ident vt
        Nothing -> modify $ \env -> env{_innerVars = M.insert ident declType (_innerVars env)}
declare declType (DeclInit p ident expr) = do
    mbInner <- gets (M.lookup ident . _innerVars)
    case mbInner of
        Just vt -> throw $ SEVariableRedeclaration p ident vt
        Nothing -> do
            et <- evalType expr
            unless (et == declType) $ throw $ SEVariableAssignmentTypeMismatch p ident et declType
            modify $ \env -> env{_innerVars = M.insert ident declType (_innerVars env)}

getVarType :: BNFC'Position -> Ident -> TypingState InternalType
getVarType p ident = do
    mbInner <- gets (M.lookup ident . _innerVars)
    mbOuter <- gets (M.lookup ident . _outerVars)
    case mergeMaybe mbInner mbOuter of
        Nothing -> throw $ SENoSuchVariable p ident
        Just vt -> return vt
  where
    mergeMaybe :: Maybe a -> Maybe a -> Maybe a
    mergeMaybe (Just x) _ = Just x
    mergeMaybe Nothing x = x

getFuncType :: BNFC'Position -> Ident -> TypingState FunctionType
getFuncType p ident = do
    mbFuncType <- gets (M.lookup ident . _funcs)
    case mbFuncType of
        Nothing -> throw $ SENoSuchFunction p ident
        Just ft -> return ft

preservingEnv :: TypingState a -> TypingState a
preservingEnv comp = do
    env <- get
    modify $ \e -> e{_outerVars = M.union (_innerVars e) (_outerVars e), _innerVars = M.empty}
    ret <- comp
    modify $ const env
    return ret

dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs S.empty
  where
    dup' [] _ = Nothing
    dup' (y : ys) s =
        if S.member y s
            then Just y
            else dup' ys (S.insert y s)