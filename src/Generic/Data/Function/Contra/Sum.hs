-- {-# LANGUAGE UndecidableInstances #-} -- due to typeclass design
{-# LANGUAGE AllowAmbiguousTypes #-}  -- due to typeclass design

module Generic.Data.Function.Contra.Sum where

import GHC.Generics
import Generic.Data.Function.Util.Generic ( conName' )
import Generic.Data.Function.Contra.Constructor ( GContraC(gContraC) )
import Generic.Data.Rep.Error
import Generic.Data.Function.Common

import Data.Functor.Contravariant.Divisible

class GContraSum (opts :: SumOpts) f g where
    gContraSum :: f String -> f (g p)

instance GContraCSum f (l :+: r) => GContraSum opts f (l :+: r) where
    gContraSum = gContraCSum

instance GContraSum 'SumOnly f (C1 c g) where
    gContraSum = error eNeedSum

instance GContraCSum f (C1 c g)
  => GContraSum 'AllowSingletonSum f (C1 c g) where
    gContraSum = gContraCSum

instance GContraSum opts f V1 where
    gContraSum = error eNoEmpty

-- TODO rename (? had this on foldmap sum)
class GContraCSum f g where gContraCSum :: f String -> f (g p)

instance (Decidable f, GContraCSum f l, GContraCSum f r)
  => GContraCSum f (l :+: r) where
    gContraCSum f = choose genericSumToEither (gContraCSum f) (gContraCSum f)
      where genericSumToEither = \case L1 l -> Left l; R1 r -> Right r

instance (Divisible f, GContraC f g, Constructor c) => GContraCSum f (C1 c g) where
    gContraCSum f = divide (\(M1 g) -> (conName' @c, g)) f gContraC
