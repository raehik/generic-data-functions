{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- for da test

module Generic.Data.Cstr where

import GHC.Generics hiding ( type R )
import GHC.TypeLits
import Data.Kind ( type Type, type Constraint )
import Generic.Type.CstrPath

class Idk tag where
    type IdkF tag :: Type -> Type
    type IdkC tag (gf :: k -> Type) :: Constraint
    idkF :: IdkC tag gf => IdkF tag (gf p)

--class GOnCstr tag (name :: Symbol) (gf :: k -> Type) where
class GOnCstr tag (name :: Symbol) gf where
    gOnCstr :: IdkF tag (gf p)

type family FromLeftE dtName cstr eae where
    FromLeftE dtName cstr (Right a) = a
    FromLeftE dtName cstr (Left  e) = TypeError
        (      Text "error searching for constructor " :<>: Text cstr
          :<>: Text " in data type " :<>: Text dtName :<>: Text ":"
          :$$: PrettyE e )

instance
  ( turns ~ FromLeftE dtName name (GCstrPath name gf)
  , Functor (IdkF tag), GOnCstr' tag turns gf
  ) => GOnCstr tag name (D1 (MetaData dtName _md2 _md3 _md4) gf) where
    {-# INLINE gOnCstr #-}
    gOnCstr = M1 <$> gOnCstr' @tag @turns

class GOnCstr' tag (turns :: [BTTurn]) gf where
    gOnCstr' :: IdkF tag (gf p)
instance (Functor (IdkF tag), GOnCstr' tag turns l)
  => GOnCstr' tag (BTL : turns) (l :+: r) where
    {-# INLINE gOnCstr' #-}
    gOnCstr' = L1 <$> gOnCstr' @tag @turns
instance (Functor (IdkF tag), GOnCstr' tag turns r)
  => GOnCstr' tag (BTR : turns) (l :+: r) where
    {-# INLINE gOnCstr' #-}
    gOnCstr' = R1 <$> gOnCstr' @tag @turns
instance (Functor (IdkF tag), Idk tag, IdkC tag gf)
  => GOnCstr' tag '[] (C1 mc gf) where
    {-# INLINE gOnCstr' #-}
    gOnCstr' = M1 <$> idkF @tag

class GX gf where
    gx :: Maybe (gf p)

instance GX U1 where gx = Nothing
instance GX (S1 c gf) where gx = Nothing
instance (GX l, GX r) => GX (l :*: r) where gx = liftA2 (:*:) gx gx

data X = X1 deriving stock (Generic, Show)

instance Idk GX where
    type IdkF GX = Maybe
    type IdkC GX gf = GX gf
    idkF = gx

gIdx
    :: forall tag (name :: Symbol) a
    .  ( Generic a, Functor (IdkF tag), GOnCstr tag name (Rep a) )
    => IdkF tag a
gIdx = to <$> gOnCstr @tag @name

{- TODO the following complains too early. TypeError messing up again?
gIdxX :: forall (name :: Symbol). Maybe X
gIdxX = to <$> gOnCstr @GX @name
gIdxX = gIdx @Bool @name @X
-}
