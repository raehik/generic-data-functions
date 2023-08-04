{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6

{- | 'foldMap' for sum types where constructors are encoded by mapping the
      constructor name.

Note that constructor names are unique per type. So as long as your mapping
function similarly outputs unique values of your monoid for each constructor,
you should be able to "reverse" the process (e.g. for generic 'traverse').
-}

module Generic.Data.Function.FoldMap.Sum where

import GHC.Generics
import Generic.Data.Function.Util.Generic ( conName' )
import Generic.Data.Function.FoldMap.Constructor ( GFoldMapC(gFoldMapC) )
import Generic.Data.Rep.Error
import Generic.Data.Function.Common

class GFoldMapSum (opts :: SumOpts) m f where gFoldMapSum :: (String -> m) -> f p -> m

instance GFoldMapCSum m (l :+: r) => GFoldMapSum opts m (l :+: r) where
    gFoldMapSum = gFoldMapCSum

instance GFoldMapSum 'SumOnly m (C1 c f) where
    gFoldMapSum = error eNeedSum

instance GFoldMapCSum m (C1 c f)
  => GFoldMapSum 'AllowSingletonSum m (C1 c f) where
    gFoldMapSum = gFoldMapCSum

instance GFoldMapSum opts m V1 where
    gFoldMapSum = error eNoEmpty

-- | Sum type handler prefixing constructor contents with their mapped
--   constructor name via a provided @String -> m@.
--
-- TODO rename
class GFoldMapCSum m f where gFoldMapCSum :: (String -> m) -> f p -> m

instance (GFoldMapCSum m l, GFoldMapCSum m r) => GFoldMapCSum m (l :+: r) where
    gFoldMapCSum f = \case L1 l -> gFoldMapCSum f l
                           R1 r -> gFoldMapCSum f r

instance (Semigroup m, Constructor c, GFoldMapC m f)
  => GFoldMapCSum m (C1 c f) where
    gFoldMapCSum mapCstr (M1 a) = mapCstr (conName' @c) <> gFoldMapC a
