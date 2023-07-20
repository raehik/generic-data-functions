{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# LANGUAGE AllowAmbiguousTypes #-} -- due to type class design
{-# LANGUAGE CPP #-} -- due to TypeErrorMessage hack

{- | 'foldMap' for sum types, where constructors are encoded by index
     (distance from first/leftmost constructor) in a single byte, which is
     prepended to their contents.

TODO. Clumsy and limited. And yet, still handy enough I think.
-}

module Generic.Data.Function.FoldMap.SumConsByte where

import GHC.Generics
import GHC.TypeLits
import Data.Kind ( Type, Constraint )
import Generic.Data.Function.Util.TypeNats ( natVal'' )
import Generic.Data.Function.Error ( type ENoEmpty, type EUnexpectedNonSum )
import Generic.Data.Function.FoldMap.Constructor ( GFoldMapC(gFoldMapC) )

import Data.Word ( Word8 )

class GFoldMapSumConsByte m f where
    gFoldMapSumConsByte :: (Word8 -> m) -> f p -> m

instance GFoldMapSumConsByte m f => GFoldMapSumConsByte m (D1 c f) where
    gFoldMapSumConsByte f (M1 a) = gFoldMapSumConsByte f a

instance
  ( FitsInByte (SumArity (l :+: r))
  , GFoldMapCSumCtrArityByte m 0 (l :+: r)
  , GFoldMapCSumCtr m (l :+: r)
  , Semigroup m
  ) => GFoldMapSumConsByte m (l :+: r) where
    gFoldMapSumConsByte f lr =
        gFoldMapCSumCtrArityByte @m @0 f lr <> gFoldMapCSumCtr lr

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

class GFoldMapCSumCtrArityByte m (arity :: Natural) f where
    gFoldMapCSumCtrArityByte :: (Word8 -> m) -> f p -> m

instance
  ( GFoldMapCSumCtrArityByte m arity l
  , GFoldMapCSumCtrArityByte m (arity + SumArity l) r
  ) => GFoldMapCSumCtrArityByte m arity (l :+: r) where
    gFoldMapCSumCtrArityByte f = \case
      L1 l -> gFoldMapCSumCtrArityByte @m @arity                f l
      R1 r -> gFoldMapCSumCtrArityByte @m @(arity + SumArity l) f r

instance KnownNat arity => GFoldMapCSumCtrArityByte m arity (C1 c f) where
    gFoldMapCSumCtrArityByte f _ = f (fromIntegral (natVal'' @arity))

---

type family SumArity (a :: Type -> Type) :: Natural where
    SumArity (C1 c a) = 1
    SumArity (x :+: y) = SumArity x + SumArity y

type FitsInByte n = FitsInByteResult (n <=? 255)

type family FitsInByteResult (b :: Bool) :: Constraint where
    FitsInByteResult 'True = ()
    FitsInByteResult 'False = TypeErrorMessage
        "TODO ya type had more than 255 constructors"

type family TypeErrorMessage (a :: Symbol) :: Constraint where
#if MIN_VERSION_base(4,9,0)
    TypeErrorMessage a = TypeError ('Text a)
-- GHC < 8.0 does not support empty closed type families
#elif __GLASGOW_HASKELL__ < 800
    TypeErrorMessage a = a ~ ""
#endif
