{-# LANGUAGE AllowAmbiguousTypes #-} -- due to type class design

module Generic.Data.Function.Contra
  ( GenericContra(..)
  , genericContraNonSum, GContraNonSum
  , genericContraSum,    GContraSum
  ) where

import GHC.Generics

import Generic.Data.Function.Contra.NonSum
import Generic.Data.Function.Contra.Sum
import Generic.Data.Function.Contra.Constructor
import Data.Functor.Contravariant

-- | Generic contra over a term of non-sum data type @a@.
--
-- @a@ must have exactly one constructor.
genericContraNonSum
    :: forall {cd} {gf} tag a
    .  ( Generic a, Rep a ~ D1 cd gf
       , GContraNonSum tag gf
       , Contravariant (GenericContraF tag))
    => GenericContraF tag a
genericContraNonSum = contramap (unM1 . from) (gContraNonSum @tag)

-- | Generic contra over a term of sum data type @a@.
--
-- You must provide a contra function for constructor names.
--
-- This is the most generic option, but depending on your string manipulation
-- may be slower.
genericContraSum
    :: forall {cd} {gf} opts tag a
    .  ( Generic a, Rep a ~ D1 cd gf
       , GContraSum opts tag gf
       , Contravariant (GenericContraF tag))
    => GenericContraF tag String
    -> GenericContraF tag a
genericContraSum f = contramap (unM1 . from) (gContraSum @opts @tag f)
