{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes #-}

{-
Argh. So this is complicated.

  * The type functions don't want to be connected to @tag@, since that's the
    serializer. Can we base them on @a@?
    * oooh. maybe we have to pass _another_ tag, for sum type handling...
  * This should work by first running the type function, then reifying the
    output of _that_ to the chosen sum tag type.
  * With that in mind, we should be able to replace the regular sum handling,
    and simply add defaults for type-level id. If that ends up being ergonomic.

This gets a bit simpler with GHC 9.10 @RequiredTypeArguments@, since we can pass
a type instead of a 'Proxy#' holding it.
-}

module Generic.Data.Function.FoldMap.SumType where

import Generic.Data.Sum
import GHC.Generics
import Data.Kind ( type Type, type Constraint )
import GHC.TypeLits ( type Symbol )
import GHC.TypeError ( ErrorMessage(..), type TypeError )
import Generic.Data.Function.Common.Generic ( absurdV1 )
import Generic.Data.Function.FoldMap.Constructor
  ( GFoldMapC(gFoldMapC)
  , GenericFoldMap(type GenericFoldMapM) )
import GHC.Exts ( type Proxy#, proxy# )

class GFoldMapSum tag sumtag gf where
    gFoldMapSum
        :: (forall
            (x :: CstrTy sumtag)
            .  ParseCstrC sumtag x
            => Proxy# x -> GenericFoldMapM tag)
        -> gf p -> GenericFoldMapM tag

instance GFoldMapSumD tag sumtag dtName gf
  => GFoldMapSum tag sumtag (D1 (MetaData dtName _md2 _md3 _md4) gf) where
    gFoldMapSum f = gFoldMapSumD @tag @sumtag @dtName f . unM1

class GFoldMapSumD tag sumtag dtName gf where
    gFoldMapSumD
        :: (forall
            (x :: CstrTy sumtag)
            .  ParseCstrC sumtag x
            => Proxy# x -> GenericFoldMapM tag)
        -> gf p -> GenericFoldMapM tag

instance GFoldMapSumD tag sumtag dtName V1 where
    gFoldMapSumD _ = absurdV1

instance GFoldMapCSum tag sumtag dtName (C1 c gf)
  => GFoldMapSumD tag sumtag dtName (C1 c gf) where
    gFoldMapSumD = gFoldMapCSum @tag @sumtag @dtName

instance GFoldMapCSum tag sumtag dtName (l :+: r)
  => GFoldMapSumD tag sumtag dtName (l :+: r) where
    gFoldMapSumD = gFoldMapCSum @tag @sumtag @dtName

class GFoldMapCSum tag sumtag dtName gf where
    gFoldMapCSum
        :: (forall
            (x :: CstrTy sumtag)
            .  ParseCstrC sumtag x
            => Proxy# x -> GenericFoldMapM tag)
        -> gf p -> GenericFoldMapM tag

instance (GFoldMapCSum tag sumtag dtName l, GFoldMapCSum tag sumtag dtName r)
  => GFoldMapCSum tag sumtag dtName (l :+: r) where
    gFoldMapCSum f = \case L1 l -> gFoldMapCSum @tag @sumtag @dtName f l
                           R1 r -> gFoldMapCSum @tag @sumtag @dtName f r

instance
  ( Semigroup (GenericFoldMapM tag), GFoldMapC tag gf
  , ParseCstrC sumtag cstrParsed
  , ForceGCParse dtName cstr (ParseCstr sumtag cstr) ~ cstrParsed
  ) => GFoldMapCSum tag sumtag dtName (C1 (MetaCons cstr _mc2 _mc3) gf) where
    gFoldMapCSum mapReifyCstr (M1 a) =
        mapReifyCstr (proxy# @cstrParsed) <> gFoldMapC @tag a

{-
instance {-# OVERLAPPING #-}
  ( GenericFoldMapSumCstrC sumtag cstrParsed
  , ParseCstr sumtag cstr ~ Left err
  , TypeError err -- TODO Unsatisfiable perhaps better!
  ) => GFoldMapCSum tag sumtag dtName (C1 (MetaCons cstr _mc2 _mc3) gf) where
    gFoldMapCSum = undefined
-}
