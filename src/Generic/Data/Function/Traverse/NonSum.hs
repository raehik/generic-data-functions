{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# LANGUAGE AllowAmbiguousTypes #-} -- due to type class design

module Generic.Data.Function.Traverse.NonSum where

import GHC.Generics
import Generic.Data.Function.Traverse.Constructor ( GTraverseC(gTraverseC) )
import Generic.Data.Rep.Error

class GTraverseNonSum (cd :: Meta) f f' where gTraverseNonSum :: f (f' p)

instance (Functor f, GTraverseC cd cc 0 f f')
  => GTraverseNonSum cd f (C1 cc f') where
    gTraverseNonSum = M1 <$> gTraverseC @cd @cc @0

instance GTraverseNonSum cd f (l :+: r) where gTraverseNonSum = error eNoSum
instance GTraverseNonSum cd f V1        where gTraverseNonSum = error eNoEmpty
