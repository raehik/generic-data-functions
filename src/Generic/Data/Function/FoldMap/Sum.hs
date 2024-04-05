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
import Generic.Data.Function.Util.Error

class GFoldMapSum tag gf where
    gFoldMapSum
        :: (String -> GenericFoldMapM tag) -> gf p -> GenericFoldMapM tag

instance GFoldMapSum tag gf => GFoldMapSum tag (D1 c gf) where
    gFoldMapSum f = gFoldMapSum @tag f . unM1

instance GFoldMapCSum tag (l :+: r) => GFoldMapSum tag (l :+: r) where
    gFoldMapSum = gFoldMapCSum @tag

instance GFoldMapCSum tag (C1 c gf) => GFoldMapSum tag (C1 c gf) where
    gFoldMapSum = gFoldMapCSum @tag

instance GFoldMapSum tag V1 where
    gFoldMapSum = error eNoEmpty

-- | Sum type handler prefixing constructor contents with their mapped
--   constructor name via a provided @String -> m@.
--
-- TODO rename
class GFoldMapCSum tag gf where
    gFoldMapCSum
        :: (String -> GenericFoldMapM tag) -> gf p -> GenericFoldMapM tag

instance (GFoldMapCSum tag l, GFoldMapCSum tag r)
  => GFoldMapCSum tag (l :+: r) where
    gFoldMapCSum f = \case L1 l -> gFoldMapCSum @tag f l
                           R1 r -> gFoldMapCSum @tag f r

instance (Semigroup (GenericFoldMapM tag), Constructor c, GFoldMapC tag gf)
  => GFoldMapCSum tag (C1 c gf) where
    gFoldMapCSum mapCstr (M1 a) = mapCstr (conName' @c) <> gFoldMapC @tag a
