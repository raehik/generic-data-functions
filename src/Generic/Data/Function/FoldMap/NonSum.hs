{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6

module Generic.Data.Function.FoldMap.NonSum where

import GHC.Generics
import Generic.Data.Function.FoldMap.Constructor ( GFoldMapC(gFoldMapC) )
import Generic.Data.Rep.Error

{- | 'foldMap' over generic product data types.

Take a generic representation, map each field in the data type to a 'Monoid',
and combine the results with ('<>').
-}
class GFoldMapNonSum m f where gFoldMapNonSum :: f p -> m

instance GFoldMapC m f => GFoldMapNonSum m (C1 c f) where
    gFoldMapNonSum (M1 a) = gFoldMapC a

instance GFoldMapNonSum m (l :+: r) where
    gFoldMapNonSum = error eNoSum

instance GFoldMapNonSum m V1 where
    gFoldMapNonSum = error eNoEmpty
