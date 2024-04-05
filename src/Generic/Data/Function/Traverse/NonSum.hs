{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# LANGUAGE AllowAmbiguousTypes #-} -- due to type class design

module Generic.Data.Function.Traverse.NonSum where

import GHC.Generics
import Generic.Data.Function.Traverse.Constructor
import Generic.Data.Function.Common.Error

class GTraverseNonSum tag gf where
    gTraverseNonSum :: GenericTraverseF tag (gf p)

instance (Functor (GenericTraverseF tag), GTraverseNonSumD tag cd gf)
  => GTraverseNonSum tag (D1 cd gf) where
    gTraverseNonSum = M1 <$> gTraverseNonSumD @tag @cd

class GTraverseNonSumD tag (cd :: Meta) gf where
    gTraverseNonSumD :: GenericTraverseF tag (gf p)

instance (Functor (GenericTraverseF tag), GTraverseC tag cd cc 0 gf)
  => GTraverseNonSumD tag cd (C1 cc gf) where
    gTraverseNonSumD = M1 <$> gTraverseC @tag @cd @cc @0

instance GTraverseNonSumD tag cd (l :+: r) where gTraverseNonSumD = error eNoSum
instance GenericTraverse tag => GTraverseNonSumD tag cd V1 where
    gTraverseNonSumD = genericTraverseV1 @tag
