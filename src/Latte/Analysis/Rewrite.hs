{-# LANGUAGE FlexibleInstances #-}

module Latte.Analysis.Rewrite where

import Latte.Grammar.Abs

class Simplified a where
    simplify :: a -> a

instance Simplified Program where
    simplify (Program p topDefs) = Program p (map simplify topDefs)

instance Simplified TopDef where
    simplify (TDFunction p retT ident args block) = TDFunction p retT ident args (simplify block)

instance Simplified StmtBlock where
    simplify (StmtBlock p stmts) = StmtBlock p (map simplify stmts)

instance Simplified Stmt where
    simplify (SBlock p block) = SBlock p (simplify block)
    simplify (SCond p (ELitFalse _) _) = SEmpty p
    simplify (SCond p (ELitTrue _) stmt) = simplify $ packStmt p stmt
    simplify (SCond p expr stmt) = SCond p expr (simplify stmt)
    simplify (SCondElse p (ELitFalse _) _ stmt) = simplify $ packStmt p stmt
    simplify (SCondElse p (ELitTrue _) stmt _) = simplify $ packStmt p stmt
    simplify (SCondElse p expr true false) = SCondElse p expr (simplify true) (simplify false)
    simplify (SWhile p (ELitFalse _) _) = SEmpty p
    simplify (SWhile p expr stmt) = SWhile p expr (simplify stmt)
    simplify stmt = stmt

packStmt :: BNFC'Position -> Stmt -> Stmt
packStmt p stmt = SBlock p (StmtBlock p [stmt])