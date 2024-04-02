{-# LANGUAGE UndecidableInstances #-} -- due to type class design
{-# LANGUAGE AllowAmbiguousTypes  #-} -- due to type class design

module Generic.Data.Function.Contra.NonSum where

import Data.Functor.Contravariant

import GHC.Generics
import Generic.Data.Function.Contra.Constructor
  ( GContraC(gContraC)
  , GenericContra(type GenericContraF)
  )
import Generic.Data.Rep.Error

class GContraNonSum tag gf where gContraNonSum :: GenericContraF tag (gf p)

instance (Contravariant (GenericContraF tag), GContraC tag g)
  => GContraNonSum tag (C1 c g) where
    gContraNonSum = contramap unM1 (gContraC @tag)

instance GContraNonSum tag (l :+: r) where gContraNonSum = error eNoSum
instance GContraNonSum tag V1        where gContraNonSum = error eNoEmpty
