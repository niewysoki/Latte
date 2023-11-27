{-# LANGUAGE FlexibleInstances #-}

module Latte.Analysis.Reachability where

data Reachability
class Reachable a where
    reachable :: a -> Bool