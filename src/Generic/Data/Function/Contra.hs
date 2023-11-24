{-# LANGUAGE AllowAmbiguousTypes #-} -- due to type class design

module Generic.Data.Function.Contra
  ( GenericContra(..)
  , genericContraNonSum, GContraNonSum
  , genericContraSum,    GContraSum
  ) where

import GHC.Generics

import Generic.Data.Rep.Assert
import Generic.Data.Function.Contra.NonSum
import Generic.Data.Function.Contra.Sum
import Generic.Data.Function.Contra.Constructor
import Data.Functor.Contravariant

-- | Generic contra over a term of non-sum data type @a@.
--
-- @a@ must have exactly one constructor.
genericContraNonSum
    :: forall {cd} {g} asserts f a
    .  ( Generic a, Rep a ~ D1 cd g
       , GContraNonSum f g
       , ApplyGCAsserts asserts g
       , Contravariant f)
    => f a
genericContraNonSum = contramap (unM1 . from) gContraNonSum

-- | Generic contra over a term of sum data type @a@.
--
-- You must provide a contra function for constructor names.
--
-- This is the most generic option, but depending on your string manipulation
-- may be slower.
genericContraSum
    :: forall {cd} {g} opts asserts f a
    .  ( Generic a, Rep a ~ D1 cd g
       , GContraSum opts f g
       , ApplyGCAsserts asserts g
       , Contravariant f)
    => (f String)
    -> f a
genericContraSum f = contramap (unM1 . from) (gContraSum @opts f)
