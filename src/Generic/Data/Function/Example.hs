{-# LANGUAGE AllowAmbiguousTypes #-}

module Generic.Data.Function.Example where

import GHC.Generics
import Generic.Data.Function.FoldMap

import Data.List qualified as List

--data D a = D1 a | D2 a a deriving stock (Generic, Show)
data X = X1 | X2 deriving stock (Generic)
data Y = Y deriving stock (Generic)

newtype Showly = Showly { unShowly :: [String] }
    --deriving Show via String
    deriving (Semigroup, Monoid) via [String]

instance GenericFoldMap Showly where
    type GenericFoldMapC Showly a = Show a
    type GenericFoldMapM Showly = [String]
    genericFoldMapF = (\a -> [a]) . show

showGeneric
    :: forall {cd} {f} opts a
    .  (Generic a, Rep a ~ D1 cd f, GFoldMapSum opts Showly f)
    => a -> String
showGeneric =
      mconcat . List.intersperse " "
    . genericFoldMapSum @opts @Showly (\cstr -> [cstr])

showGeneric'
    :: forall {cd} {f} a
    .  (Generic a, Rep a ~ D1 cd f, GFoldMapNonSum Showly f)
    => a -> String
showGeneric' =
    mconcat . List.intersperse " " . genericFoldMapNonSum @Showly
