{-# LANGUAGE UndecidableInstances #-} -- due to type class design
{-# LANGUAGE AllowAmbiguousTypes  #-} -- due to type class design

module Generic.Data.Function.Contra.NonSum where

import Data.Functor.Contravariant

import GHC.Generics
import Generic.Data.Function.Contra.Constructor
  ( GContraC(gContraC)
  , GenericContra(type GenericContraF)
  )
import Generic.Data.Function.Util.Error

class GContraNonSum tag gf where gContraNonSum :: GenericContraF tag (gf p)

instance (Contravariant (GenericContraF tag), GContraNonSumD tag gf)
  => GContraNonSum tag (C1 c gf) where
    gContraNonSum = contramap unM1 (gContraNonSumD @tag)

class GContraNonSumD tag gf where gContraNonSumD :: GenericContraF tag (gf p)

instance (Contravariant (GenericContraF tag), GContraC tag gf)
  => GContraNonSumD tag (C1 c gf) where
    gContraNonSumD = contramap unM1 (gContraC @tag)

instance GContraNonSumD tag (l :+: r) where gContraNonSumD = error eNoSum
instance GContraNonSumD tag V1        where gContraNonSumD = error eNoEmpty
