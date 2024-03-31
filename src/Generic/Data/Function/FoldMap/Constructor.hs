{-# LANGUAGE AllowAmbiguousTypes  #-} -- due to tag type class design
{-# LANGUAGE UndecidableInstances #-} -- due to generic type class design

module Generic.Data.Function.FoldMap.Constructor where

import GHC.Generics
import Data.Kind ( type Constraint, type Type )

import Generic.Data.Function.Via
import GHC.TypeLits ( TypeError )

-- | 'Monoid's that can be generically 'foldMap'ped to.
class GenericFoldMap tag where
    -- | The target 'Monoid' to 'foldMap' to.
    type GenericFoldMapM tag :: Type

    -- | The type class that enables mapping permitted types to the monoid.
    --
    -- The type class should provide a function that looks like
    -- 'genericFoldMapF'.
    type GenericFoldMapC tag a :: Constraint

    -- | The "map" function in 'foldMap' (first argument).
    genericFoldMapF :: GenericFoldMapC tag a => a -> GenericFoldMapM tag

-- | 'foldMap' over types with no fields in any constructor.
instance GenericFoldMap (NoRec0 m) where
    type GenericFoldMapM (NoRec0 m)   = m
    type GenericFoldMapC (NoRec0 m) _ = TypeError ENoRec0
    genericFoldMapF = undefined
    -- ^ TODO why safe

-- | 'foldMap' over types where all fields map to 'mempty'.
instance Monoid m => GenericFoldMap (EmptyRec0 m) where
    type GenericFoldMapM (EmptyRec0 m)   = m
    type GenericFoldMapC (EmptyRec0 m) _ = ()
    genericFoldMapF _ = mempty

-- | 'foldMap' on individual constructors (products).
class GFoldMapC tag f where gFoldMapC :: f p -> GenericFoldMapM tag

-- | 'foldMap' on individual constructors (products).
instance (Semigroup (GenericFoldMapM tag), GFoldMapC tag l, GFoldMapC tag r)
  => GFoldMapC tag (l :*: r) where
    gFoldMapC (l :*: r) = gFoldMapC @tag l <> gFoldMapC @tag r

instance (GenericFoldMap tag, GenericFoldMapC tag a)
  => GFoldMapC tag (S1 c (Rec0 a)) where
    gFoldMapC (M1 (K1 a)) = genericFoldMapF @tag a

-- | Wow, look! Nothing!
instance Monoid (GenericFoldMapM tag) => GFoldMapC tag U1 where
    gFoldMapC U1 = mempty
