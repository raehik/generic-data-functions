module Tmp where

import GHC.Generics
import Generic.Data.FOnCstr

class GX gf where
    gx :: Maybe (gf p)

instance GX U1 where gx = Nothing
instance GX (S1 c gf) where gx = Nothing
instance (GX l, GX r) => GX (l :*: r) where gx = liftA2 (:*:) gx gx

data X = X1 deriving stock (Generic, Show)

instance GenericFOnCstr GX where
    type GenericFOnCstrF GX = Maybe
    type GenericFOnCstrC GX gf = GX gf
    genericFOnCstrF = gx
