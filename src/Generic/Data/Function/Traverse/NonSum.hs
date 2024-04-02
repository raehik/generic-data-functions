{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# LANGUAGE AllowAmbiguousTypes #-} -- due to type class design

module Generic.Data.Function.Traverse.NonSum where

import GHC.Generics
import Generic.Data.Function.Traverse.Constructor
  ( GTraverseC(gTraverseC)
  , GenericTraverse(type GenericTraverseF)
  )
import Generic.Data.Rep.Error

class GTraverseNonSum (cd :: Meta) tag gf where
    gTraverseNonSum :: GenericTraverseF tag (gf p)

instance (Functor (GenericTraverseF tag), GTraverseC cd cc 0 tag gf)
  => GTraverseNonSum cd tag (C1 cc gf) where
    gTraverseNonSum = M1 <$> gTraverseC @cd @cc @0 @tag

instance GTraverseNonSum cd tag (l :+: r) where gTraverseNonSum = error eNoSum
instance GTraverseNonSum cd tag V1        where gTraverseNonSum = error eNoEmpty
