{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# LANGUAGE AllowAmbiguousTypes #-} -- due to type class design
{-# LANGUAGE CPP #-} -- due to TypeErrorMessage hack

{- | 'foldMap' for sum types, where constructors are encoded by index
     (distance from leftmost constructor).

TODO the @Natural -> m@ function may be inefficient. If so, I don't know how to
re-efficient it. If I could write something more like @forall n. KnownNat n =>
m@, I think it might be better. But I shouldn't write a class for it because
it's only concerned with the SOP representation, so should be pluggable.

TODO FitsInByte is worthless oops. Could I change this to provide some extra
type safety while being super flexible?
-}

module Generic.Data.Function.FoldMap.SumConsByte where

import GHC.Generics
import GHC.TypeLits
import Data.Kind ( Type, Constraint )
import Generic.Data.Function.Util.TypeNats ( natVal'' )
import Generic.Data.Function.Error ( type ENoEmpty, type EUnexpectedNonSum )
import Generic.Data.Function.FoldMap.Constructor ( GFoldMapC(gFoldMapC) )

class GFoldMapSumConsByte m f where
    gFoldMapSumConsByte :: (Natural -> m) -> f p -> m

instance GFoldMapSumConsByte m f => GFoldMapSumConsByte m (D1 c f) where
    gFoldMapSumConsByte f (M1 a) = gFoldMapSumConsByte f a

instance
  ( FitsInByte (SumArity (l :+: r))
  , G m 0 (l :+: r)
  , GFoldMapCSumCtr m (l :+: r)
  , Semigroup m
  ) => GFoldMapSumConsByte m (l :+: r) where
    gFoldMapSumConsByte f lr = g @m @0 f lr <> gFoldMapCSumCtr lr

instance TypeError EUnexpectedNonSum => GFoldMapSumConsByte m (C1 c f) where
    gFoldMapSumConsByte _ = undefined

instance TypeError ENoEmpty => GFoldMapSumConsByte m V1 where
    gFoldMapSumConsByte _ = undefined

---

-- | Sum type handler handling constructors only. Useful if you handle
--   constructor prefixes elsewhere.
class GFoldMapCSumCtr m f where gFoldMapCSumCtr :: f p -> m

instance (GFoldMapCSumCtr m l, GFoldMapCSumCtr m r)
  => GFoldMapCSumCtr m (l :+: r) where
    gFoldMapCSumCtr = \case L1 l -> gFoldMapCSumCtr l
                            R1 r -> gFoldMapCSumCtr r

instance GFoldMapC m f => GFoldMapCSumCtr m (C1 c f) where
    gFoldMapCSumCtr (M1 a) = gFoldMapC a

---

class G m (arity :: Natural) f where
    g :: (Natural -> m) -> f p -> m

instance (G m arity l, G m (arity + SumArity l) r)
  => G m arity (l :+: r) where
    g f = \case L1 l -> g @m @arity                f l
                R1 r -> g @m @(arity + SumArity l) f r

instance KnownNat arity => G m arity (C1 c f) where
    g f _ = f (natVal'' @arity)

---

type family SumArity (a :: Type -> Type) :: Natural where
    SumArity (C1 c a) = 1
    SumArity (x :+: y) = SumArity x + SumArity y

type FitsInByte n = FitsInByteResult (n <=? 255)

type family FitsInByteResult (b :: Bool) :: Constraint where
    FitsInByteResult 'True = ()
    FitsInByteResult 'False = TypeErrorMessage
        "Generic deriving of Store instances can only be used on datatypes with fewer than 256 constructors."

type family TypeErrorMessage (a :: Symbol) :: Constraint where
#if MIN_VERSION_base(4,9,0)
    TypeErrorMessage a = TypeError ('Text a)
-- GHC < 8.0 does not support empty closed type families
#elif __GLASGOW_HASKELL__ < 800
    TypeErrorMessage a = a ~ ""
#endif
