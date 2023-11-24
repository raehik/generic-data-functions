module Generic.Data.Function.Contra.NonSum where

import Data.Functor.Contravariant

import GHC.Generics
import Generic.Data.Function.Contra.Constructor ( GContraC(gContraC) )
import Generic.Data.Rep.Error

class GContraNonSum f g where gContraNonSum :: f (g p)

instance (Contravariant f, GContraC f g)
  => GContraNonSum f (C1 c g) where
    gContraNonSum = contramap unM1 gContraC

instance GContraNonSum f (l :+: r) where gContraNonSum = error eNoSum
instance GContraNonSum f V1        where gContraNonSum = error eNoEmpty
