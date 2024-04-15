-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Generic.Data.Function.Example.FoldMapSumType where

import Generic.Data.Function.FoldMap
import Generic.Data.Function.FoldMap.SumType
import GHC.Generics
import Data.Type.Symbol
import Data.Type.Symbol.Natural
import Data.Word
import GHC.TypeNats
import Generic.Data.Wrappers ( type NoRec0 )
import GHC.TypeLits ( type Symbol )
import Data.Monoid ( Sum(..) )
import GHC.Exts ( type Proxy# )

data DEnum3 = DEnum1 | DEnum2 | DEnum3 deriving stock Generic

data DEnum3G
instance XYZInner DEnum3G where
    type GenericFoldMapSumCstrTy DEnum3G = Natural
$(return [])
instance XYZ DEnum3G where
    type XYZCstrTo DEnum3G sym = HexSymbolToNat (Drop 5 sym)
    type GenericFoldMapSumCstrC DEnum3G n = KnownNat n

asdf :: DEnum3 -> Natural
asdf =
    getSum . genericFoldMapSumType @(NoRec0 (Sum Natural)) @DEnum3G (\p -> Sum (natVal' p))
