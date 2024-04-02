{-# LANGUAGE UndecidableInstances #-} -- due to typeclass design
{-# LANGUAGE AllowAmbiguousTypes #-}  -- due to typeclass design

module Generic.Data.Function.Contra.Sum where

import GHC.Generics
import Generic.Data.Function.Util.Generic ( conName' )
import Generic.Data.Function.Contra.Constructor
  ( GContraC(gContraC)
  , GenericContra(type GenericContraF)
  )
import Generic.Data.Rep.Error
import Generic.Data.Function.Common

import Data.Functor.Contravariant.Divisible

class GContraSum (opts :: SumOpts) tag gf where
    gContraSum :: GenericContraF tag String -> GenericContraF tag (gf p)

instance GContraCSum tag (l :+: r) => GContraSum opts tag (l :+: r) where
    gContraSum = gContraCSum @tag

instance GContraSum 'SumOnly tag (C1 c g) where
    gContraSum = error eNeedSum

instance GContraCSum tag (C1 c g)
  => GContraSum 'AllowSingletonSum tag (C1 c g) where
    gContraSum = gContraCSum @tag

instance GContraSum opts tag V1 where
    gContraSum = error eNoEmpty

-- TODO rename (? had this on foldmap sum)
class GContraCSum tag gf where
    gContraCSum :: GenericContraF tag String -> GenericContraF tag (gf p)

instance
  ( Decidable (GenericContraF tag)
  , GContraCSum tag l
  , GContraCSum tag r
  ) => GContraCSum tag (l :+: r) where
    gContraCSum f = choose genericSumToEither (gContraCSum @tag f) (gContraCSum @tag f)
      where genericSumToEither = \case L1 l -> Left l; R1 r -> Right r

instance
  ( Divisible (GenericContraF tag)
  , GContraC tag gf, Constructor c
  ) => GContraCSum tag (C1 c gf) where
    gContraCSum f = divide (\(M1 g) -> (conName' @c, g)) f (gContraC @tag)
