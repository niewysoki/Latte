{-# LANGUAGE FlexibleInstances #-}

module Latte.Analysis.ReturnAnalysis where

import Control.Monad (when)
import qualified Data.List as L
import Data.Maybe (fromJust, isJust)
import Latte.Analysis.Data (InternalType (ITVoid), fromType)
import Latte.Analysis.Exceptions (StaticException (SENoReturn), throw)
import Latte.Grammar.Abs (
    Expr' (..),
    Ident (..),
    Program,
    Program' (Program),
    Stmt,
    Stmt' (..),
    StmtBlock,
    StmtBlock' (StmtBlock),
    TopDef,
    TopDef' (TDFunction),
 )
import Latte.Grammar.ErrM (Err)

checkReturn :: Program -> Err ()
checkReturn (Program _ topDefs) =
    let mbNoReturn = L.find (not . returns) topDefs
        mbFunName = (\(TDFunction _ _ ident _ _) -> ident) <$> mbNoReturn
     in when (isJust mbFunName) $ throw $ SENoReturn (fromJust mbFunName)

class Returns a where
    returns :: a -> Bool

instance Returns TopDef where
    returns (TDFunction _ rt _ _ block) = (fromType rt == ITVoid) || returns block

instance Returns StmtBlock where
    returns (StmtBlock _ stmts) = any returns stmts

instance Returns Stmt where
    returns (SBlock _ block) = returns block
    returns (SCondElse _ _ true false) = returns true && returns false
    returns (SWhile _ (ELitTrue _) _) = True
    returns (SWhile _ _ stmt) = returns stmt
    returns (SRet{}) = True
    returns (SVRet{}) = True
    returns (SExp _ (EApp _ (EVar _ (Ident "error")) [])) = True
    returns _ = False