{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6

module Generic.Data.Function.FoldMap.Sum where

import GHC.Generics
import GHC.TypeLits ( TypeError )
import Generic.Data.Function.Util.Generic ( conName' )
import Generic.Data.Function.Error ( type ENoEmpty, type EUnexpectedNonSum )
import Generic.Data.Function.FoldMap.Constructor ( GFoldMapC(gFoldMapC) )

class GFoldMapSum m f where
    gFoldMapSum :: (String -> m) -> f p -> m

instance GFoldMapSum m f => GFoldMapSum m (D1 c f) where
    gFoldMapSum f (M1 a) = gFoldMapSum f a

instance GFoldMapCSum m (l :+: r) => GFoldMapSum m (l :+: r) where
    gFoldMapSum = gFoldMapCSum

instance TypeError EUnexpectedNonSum => GFoldMapSum m (C1 c f) where
    gFoldMapSum = undefined

instance TypeError ENoEmpty => GFoldMapSum m V1 where
    gFoldMapSum = undefined

class GFoldMapCSum m f where gFoldMapCSum :: (String -> m) -> f p -> m

instance (GFoldMapCSum m l, GFoldMapCSum m r) => GFoldMapCSum m (l :+: r) where
    gFoldMapCSum f = \case L1 l -> gFoldMapCSum f l
                           R1 r -> gFoldMapCSum f r

instance (Semigroup m, Constructor c, GFoldMapC m f)
  => GFoldMapCSum m (C1 c f) where
    gFoldMapCSum mapCstr (M1 a) = mapCstr (conName' @c) <> gFoldMapC a
