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

data DropHex (n :: Natural)
instance XYZInner (DropHex n) where
    type GenericFoldMapSumCstrTy (DropHex n) = Natural
-- required TH splice due to GHC bug with dependent type families
$(return [])
instance XYZ (DropHex n) where
    type ParseCstr (DropHex n) sym =
        MapLeftPrettyE (ParseHexSymbol (Drop n sym))
    type GenericFoldMapSumCstrC (DropHex n) cstrHexNat = KnownNat cstrHexNat

asdf :: DEnum3 -> Natural
asdf =
    getSum . genericFoldMapSumType @(NoRec0 (Sum Natural)) @(DropHex 5) (\p -> Sum (natVal' p))

-- Note that we need a monoid even though we could skip it, given that we can
-- just use the sum handler. This seems like something to tweak. Indeed, perhaps
-- I drop the monoid requirement for sum types...? Match on @C1 c U1@ specially.
