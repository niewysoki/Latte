module Latte.Analysis.StaticAnalysis where

import Latte.Analysis.Typing (checkTypes)
import Latte.Grammar.Abs (Program)
import Latte.Grammar.ErrM (Err)

run :: Program -> Err ()
run = checkTypes
