module Generic.Data.Function.Common where

import GHC.Generics
import Data.Kind

-- This works nicely if you want to keep your type signatures clean. But, it's
-- honestly so nothing. Why wouldn't you just put this in each type signature?
type GenericUnD1 :: Type -> (Type -> Type) -> Meta -> Constraint
type GenericUnD1 a f cd = (Generic a, Rep a ~ D1 cd f)

-- Below works, but too clunky.
class UnD1C (a :: k -> Type) where
    type UnD1 a :: k -> Type
    unD1 :: a p -> (UnD1 a) p

instance UnD1C (D1 cd f) where
    type UnD1 (D1 cd f) = f
    unD1 = unM1

type family UnD1' a where UnD1' (D1 cd f) = f
