{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol where

import GHC.TypeLits

type Drop n sym = Drop' n (UnconsSymbol sym)
type family Drop' n sym where
    Drop' 0 ('Just '(ch, sym)) = ConsSymbol ch sym
    Drop' 0 'Nothing = ""
    Drop' n 'Nothing = TypeError ('Text "tried to drop from empty symbol")
    Drop' n ('Just '(_, sym)) = Drop' (n-1) (UnconsSymbol sym)
