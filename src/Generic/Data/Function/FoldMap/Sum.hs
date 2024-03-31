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
import Generic.Data.Function.FoldMap.Constructor
  ( GFoldMapC(gFoldMapC)
  , GenericFoldMap(type GenericFoldMapM) )
import Generic.Data.Rep.Error
import Generic.Data.Function.Common

class GFoldMapSum (opts :: SumOpts) tag f where
    gFoldMapSum :: (String -> GenericFoldMapM tag) -> f p -> GenericFoldMapM tag

instance GFoldMapCSum tag (l :+: r) => GFoldMapSum opts tag (l :+: r) where
    gFoldMapSum = gFoldMapCSum @tag

instance GFoldMapSum 'SumOnly tag (C1 c f) where
    gFoldMapSum = error eNeedSum

instance GFoldMapCSum tag (C1 c f)
  => GFoldMapSum 'AllowSingletonSum tag (C1 c f) where
    gFoldMapSum = gFoldMapCSum @tag

instance GFoldMapSum opts tag V1 where
    gFoldMapSum = error eNoEmpty

-- | Sum type handler prefixing constructor contents with their mapped
--   constructor name via a provided @String -> m@.
--
-- TODO rename
class GFoldMapCSum tag f where
    gFoldMapCSum :: (String -> GenericFoldMapM tag) -> f p -> GenericFoldMapM tag

instance (GFoldMapCSum tag l, GFoldMapCSum tag r)
  => GFoldMapCSum tag (l :+: r) where
    gFoldMapCSum f = \case L1 l -> gFoldMapCSum @tag f l
                           R1 r -> gFoldMapCSum @tag f r

instance (Semigroup (GenericFoldMapM tag), Constructor c, GFoldMapC tag f)
  => GFoldMapCSum tag (C1 c f) where
    gFoldMapCSum mapCstr (M1 a) = mapCstr (conName' @c) <> gFoldMapC @tag a
