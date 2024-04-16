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
import GHC.TypeError ( ErrorMessage(..), type TypeError )
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
    -- yeah GHC bug. gotta add a TH splice between xyzinner and xyz. lmao
    type GenericFoldMapSumCstrC sumtag (x :: GenericFoldMapSumCstrTy sumtag)
        :: Constraint

    -- | Constructor parser.
    --
    -- By requesting a fallible parser, we can annotate the error with generic
    -- information (datatype name, constructor) before emitting.
    type ParseCstr sumtag (sym :: Symbol)
        :: Either ErrorMessage (GenericFoldMapSumCstrTy sumtag)
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

instance GFoldMapSumD tag sumtag dtName gf
  => GFoldMapSum tag sumtag (D1 ('MetaData dtName _md2 _md3 _md4) gf) where
    gFoldMapSum f = gFoldMapSumD @tag @sumtag @dtName f . unM1

class GFoldMapSumD tag sumtag dtName gf where
    gFoldMapSumD
        :: (forall
            (x :: GenericFoldMapSumCstrTy sumtag)
            .  GenericFoldMapSumCstrC sumtag x
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
            (x :: GenericFoldMapSumCstrTy sumtag)
            .  GenericFoldMapSumCstrC sumtag x
            => Proxy# x -> GenericFoldMapM tag)
        -> gf p -> GenericFoldMapM tag

instance (GFoldMapCSum tag sumtag dtName l, GFoldMapCSum tag sumtag dtName r)
  => GFoldMapCSum tag sumtag dtName (l :+: r) where
    gFoldMapCSum f = \case L1 l -> gFoldMapCSum @tag @sumtag @dtName f l
                           R1 r -> gFoldMapCSum @tag @sumtag @dtName f r

type family RunParser dtName cstr a where
    RunParser _      _    ('Right a) = a
    RunParser dtName cstr ('Left  e) = TypeError
      ( 'Text "error while parsing "
        :<>: 'Text dtName :<>: 'Text "." :<>: 'Text cstr :<>: 'Text ":"
        :$$: e
      )

instance
  ( Semigroup (GenericFoldMapM tag), GFoldMapC tag gf
  , GenericFoldMapSumCstrC sumtag cstrParsed
  , RunParser dtName cstr (ParseCstr sumtag cstr) ~ cstrParsed
  ) => GFoldMapCSum tag sumtag dtName (C1 ('MetaCons cstr _mc2 _mc3) gf) where
    gFoldMapCSum mapReifyCstr (M1 a) =
        mapReifyCstr (proxy# @cstrParsed) <> gFoldMapC @tag a

{-
instance {-# OVERLAPPING #-}
  ( GenericFoldMapSumCstrC sumtag cstrParsed
  , ParseCstr sumtag cstr ~ Left err
  , TypeError err -- TODO Unsatisfiable perhaps better!
  ) => GFoldMapCSum tag sumtag dtName (C1 ('MetaCons cstr _mc2 _mc3) gf) where
    gFoldMapCSum = undefined
-}
