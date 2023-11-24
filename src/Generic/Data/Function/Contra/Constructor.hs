{- TODO
* I use conquer in general, but for EmptyRec0 I need to use mempty. Feels a
  little weird. conquer *should* be the same as mempty, but I'm not sure we can
  guarantee it.
-}

{-# LANGUAGE UndecidableInstances #-} -- due to type class design

module Generic.Data.Function.Contra.Constructor where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

import GHC.Generics
import Data.Kind ( type Constraint )

import Generic.Data.Function.Via
import GHC.TypeLits ( TypeError )

class GenericContra f where
    -- TODO probs don't want the a ?
    type GenericContraC f a :: Constraint
    genericContraF :: GenericContraC f a => f a

-- | over types with no fields in any constructor
instance GenericContra NoRec0 where
    type GenericContraC NoRec0 _ = TypeError ENoRec0
    genericContraF = undefined

-- | over types where all fields map to 'mempty'
instance GenericContra EmptyRec0 where
    type GenericContraC EmptyRec0 a = Monoid a
    genericContraF = EmptyRec0 mempty

class GContraC f g where gContraC :: f (g p)

instance (Divisible f, GContraC f l, GContraC f r)
  => GContraC f (l :*: r) where
    gContraC = divide (\(l :*: r) -> (l, r)) gContraC gContraC

instance (Contravariant f, GenericContra f, GenericContraC f a) => GContraC f (S1 c (Rec0 a)) where
    gContraC = contramap (unK1 . unM1) genericContraF

instance Divisible f => GContraC f U1 where gContraC = conquer
