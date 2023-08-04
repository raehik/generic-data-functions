{-# LANGUAGE AllowAmbiguousTypes #-}

module Generic.Data.Function.Example where

import GHC.Generics
import Generic.Data.Function.FoldMap
import Generic.Data.Rep.Assert

import Data.List qualified as List

--data D a = D1 a | D2 a a deriving stock (Generic, Show)
data X = X1 | X2 deriving stock (Generic)
data Y = Y deriving stock (Generic)

newtype Showly = Showly { unShowly :: [String] }
    --deriving Show via String
    deriving (Semigroup, Monoid) via [String]

instance GenericFoldMap Showly where
    type GenericFoldMapC Showly a = Show a
    genericFoldMapF = Showly . (\a -> [a]) . show

showGeneric
    :: forall {cd} {f} opts asserts a
    .  (Generic a, Rep a ~ D1 cd f, GFoldMapSum opts Showly f, ApplyGCAsserts asserts f)
    => a -> String
showGeneric =
      mconcat . List.intersperse " " . unShowly
    . genericFoldMapSum @opts @asserts (\cstr -> Showly [cstr])

showGeneric'
    :: forall {cd} {f} asserts a
    .  (Generic a, Rep a ~ D1 cd f, GFoldMapNonSum Showly f, ApplyGCAsserts asserts f)
    => a -> String
showGeneric' =
    mconcat . List.intersperse " " . unShowly . genericFoldMapNonSum @asserts
