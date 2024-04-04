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
    :: forall opts a
    .  (Generic a, GFoldMapSum Showly opts (Rep a))
    => a -> String
showGeneric =
      mconcat . List.intersperse " "
    . genericFoldMapSum @Showly @opts (\cstr -> [cstr])

showGeneric'
    :: forall a
    .  (Generic a, GFoldMapNonSum Showly (Rep a))
    => a -> String
showGeneric' =
    mconcat . List.intersperse " " . genericFoldMapNonSum @Showly
