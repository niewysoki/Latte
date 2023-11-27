module Latte.Analysis.StaticAnalysis where

import Latte.Analysis.ReturnAnalysis (checkReturn)
import Latte.Analysis.Rewrite (simplify)
import Latte.Analysis.Typing (checkTypes)
import Latte.Grammar.Abs (Program)
import Latte.Grammar.ErrM (Err)

run :: Program -> Err ()
run program = do
    checkTypes program
    let program' = simplify program
    checkReturn program'