{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- for da test

module Generic.Data.FOnCstr where

import GHC.Generics
import GHC.TypeLits
import Data.Kind ( type Type, type Constraint )
import Generic.Type.CstrPath

-- | What generic functor to run on the requested constructor.
class GenericFOnCstr tag where
    -- | Functor.
    type GenericFOnCstrF tag :: Type -> Type

    -- | Constraint.
    type GenericFOnCstrC tag (gf :: k -> Type) :: Constraint

    -- | Generic functor.
    genericFOnCstrF :: GenericFOnCstrC tag gf => GenericFOnCstrF tag (gf p)

-- | Run a generic functor (provided via @tag@) on the constructor name @name@.
--
-- We hope and pray that GHC removes the generic wrappers, at least the
-- constructor ones, since we do a whole bunch of nothing with them on the term
-- level. Checking this is a big TODO.
class GFOnCstr tag (name :: Symbol) gf where
    gFOnCstr :: GenericFOnCstrF tag (gf p)

type family FromLeftE dtName cstr eae where
    FromLeftE dtName cstr (Right a) = a
    FromLeftE dtName cstr (Left  e) = TypeError
        (      Text "error searching for constructor " :<>: Text cstr
          :<>: Text " in data type " :<>: Text dtName :<>: Text ":"
          :$$: e )

instance
  ( turns ~ FromLeftE dtName name (GCstrPath name gf)
  , Functor (GenericFOnCstrF tag), GFOnCstr' tag turns gf
  ) => GFOnCstr tag name (D1 (MetaData dtName _md2 _md3 _md4) gf) where
    {-# INLINE gFOnCstr #-}
    gFOnCstr = M1 <$> gFOnCstr' @tag @turns

class GFOnCstr' tag (turns :: [GCstrChoice]) gf where
    gFOnCstr' :: GenericFOnCstrF tag (gf p)

{-
The following instances rely on @turns@ being valid for the current generic
representation. I don't attempt to handle this at all, because I assume my
constructor path algorithm is correct. Please let me know if you get an error
that says it came from this class.
-}
instance (Functor (GenericFOnCstrF tag), GFOnCstr' tag turns l)
  => GFOnCstr' tag (GoL1 : turns) (l :+: r) where
    {-# INLINE gFOnCstr' #-}
    gFOnCstr' = L1 <$> gFOnCstr' @tag @turns
instance (Functor (GenericFOnCstrF tag), GFOnCstr' tag turns r)
  => GFOnCstr' tag (GoR1 : turns) (l :+: r) where
    {-# INLINE gFOnCstr' #-}
    gFOnCstr' = R1 <$> gFOnCstr' @tag @turns
instance
  ( Functor (GenericFOnCstrF tag), GenericFOnCstr tag, GenericFOnCstrC tag gf
  ) => GFOnCstr' tag '[] (C1 mc gf) where
    {-# INLINE gFOnCstr' #-}
    gFOnCstr' = M1 <$> genericFOnCstrF @tag

-- | Run a generic functor on the requested constructor of the given type.
genericFOnCstr
    :: forall tag (name :: Symbol) a
    .  ( Generic a, Functor (GenericFOnCstrF tag), GFOnCstr tag name (Rep a) )
    => GenericFOnCstrF tag a
genericFOnCstr = to <$> gFOnCstr @tag @name

{-
This type errors due to a "could not deduce 'GFOnCstr GX name (Rep a)'", "The
type variable 'k0' is ambiguous". Unsure why. GHC seems to indicate that it's
making a whole bunch of unused k1-3 type vars... I feel like GHC is trying to
match instances too early, and somehow backs itself into a corner.
genericFOnCstr'
    :: forall (name :: Symbol) a
    .  ( Generic a, GFOnCstr GX name (Rep a) )
    => Maybe a
genericFOnCstr' = to <$> gFOnCstr @GX @name
-}
