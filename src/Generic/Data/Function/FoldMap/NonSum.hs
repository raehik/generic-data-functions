{-# LANGUAGE AllowAmbiguousTypes  #-} -- due to tag type class design
{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6

module Generic.Data.Function.FoldMap.NonSum where

import GHC.Generics
import Generic.Data.Function.FoldMap.Constructor
  ( GFoldMapC(gFoldMapC)
  , GenericFoldMap(type GenericFoldMapM) )
import Generic.Data.Rep.Error

{- | 'foldMap' over generic product data types.

Take a generic representation, map each field in the data type to a 'Monoid',
and combine the results with ('<>').
-}
class GFoldMapNonSum tag f where gFoldMapNonSum :: f p -> GenericFoldMapM tag

instance GFoldMapC tag f => GFoldMapNonSum tag (C1 c f) where
    gFoldMapNonSum (M1 a) = gFoldMapC @tag a

instance GFoldMapNonSum tag (l :+: r) where gFoldMapNonSum = error eNoSum
instance GFoldMapNonSum tag V1        where gFoldMapNonSum = error eNoEmpty
