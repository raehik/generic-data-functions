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
-}

module Generic.Data.Function.FoldMap.SumType where

import GHC.Generics
import Data.Kind ( type Type, type Constraint )
import GHC.TypeLits ( type Symbol )
import Generic.Data.Function.Common.Generic ( absurdV1 )
import Generic.Data.Function.FoldMap.Constructor
  ( GFoldMapC(gFoldMapC)
  , GenericFoldMap(type GenericFoldMapM) )
import GHC.Exts ( type Proxy#, proxy#, type ZeroBitType )

-- can't put these under a single type class due to "type constructor cannot be
-- used here (it is defined and used in the same recursive group)" :(
class XYZInner sumtag where
    type GenericFoldMapSumCstrTy sumtag

class XYZInner sumtag => XYZ sumtag where
    -- TODO this apparently doesn't work...? specifically when writing
    -- instances, the type family doesn't get expanded
    -- uhhhh not sure why. MWE works as expected.
    -- OK it seems to depend on if you write the instance in the module you
    -- define the type family or not? WTF???? GHC bug?
    type GenericFoldMapSumCstrC sumtag (x :: GenericFoldMapSumCstrTy sumtag)
        :: Constraint
    type XYZCstrTo sumtag (sym :: Symbol) :: GenericFoldMapSumCstrTy sumtag
        {-
    type XYZReified sumtag :: Type
    xyzReify
        :: forall proxy (x :: GenericFoldMapSumCstrTy sumtag)
        .  proxy x -> XYZReified sumtag
        -}

class GFoldMapSum tag sumtag gf where
    gFoldMapSum
        :: (forall
            (x :: GenericFoldMapSumCstrTy sumtag)
            .  GenericFoldMapSumCstrC sumtag x
            => Proxy# x -> GenericFoldMapM tag)
        -> gf p -> GenericFoldMapM tag

instance GFoldMapSum tag sumtag gf
  => GFoldMapSum tag sumtag (D1 c gf) where
    gFoldMapSum f = gFoldMapSum @tag @sumtag f . unM1

instance GFoldMapCSum tag sumtag (l :+: r)
  => GFoldMapSum tag sumtag (l :+: r) where
    gFoldMapSum = gFoldMapCSum @tag @sumtag

instance GFoldMapCSum tag sumtag (C1 c gf)
  => GFoldMapSum tag sumtag (C1 c gf) where
    gFoldMapSum = gFoldMapCSum @tag @sumtag

instance GFoldMapSum tag sumtag V1 where
    gFoldMapSum _ = absurdV1

class GFoldMapCSum tag sumtag gf where
    gFoldMapCSum
        :: (forall
            (x :: GenericFoldMapSumCstrTy sumtag)
            .  GenericFoldMapSumCstrC sumtag x
            => Proxy# x -> GenericFoldMapM tag)
        -> gf p -> GenericFoldMapM tag

instance (GFoldMapCSum tag sumtag l, GFoldMapCSum tag sumtag r)
  => GFoldMapCSum tag sumtag (l :+: r) where
    gFoldMapCSum f = \case L1 l -> gFoldMapCSum @tag @sumtag f l
                           R1 r -> gFoldMapCSum @tag @sumtag f r

instance
  ( Semigroup (GenericFoldMapM tag), GFoldMapC tag gf
  , GenericFoldMapSumCstrC sumtag (XYZCstrTo sumtag cstr)
  ) => GFoldMapCSum tag sumtag (C1 ('MetaCons cstr _mc2 _mc3) gf) where
    gFoldMapCSum mapReifyCstr (M1 a) =
        mapReifyCstr (proxy# @(XYZCstrTo sumtag cstr)) <> gFoldMapC @tag a
