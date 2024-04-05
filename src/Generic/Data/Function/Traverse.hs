{-# LANGUAGE AllowAmbiguousTypes #-}

{- | 'traverse' for generic data types.

TODO This is harder to conceptualize than generic 'foldMap'. No nice clean
explanation yet.

This function can provide generic support for simple parser-esque types.
-}

module Generic.Data.Function.Traverse
  ( GenericTraverse(..)
  , genericTraverseNonSum , GTraverseNonSum
  , GenericTraverseSum(..), PfxTagCfg(..)
  , genericTraverseSum,     GTraverseSum
  , eqShowPfxTagCfg
  ) where

import GHC.Generics

import Generic.Data.Function.Traverse.NonSum
import Generic.Data.Function.Traverse.Sum
import Generic.Data.Function.Traverse.Constructor

import Data.Text qualified as Text

-- | Generic 'traverse' over a term of non-sum data type @f a@,
--   where @f@ is set by the @tag@ you pass.
genericTraverseNonSum
    :: forall {k} (tag :: k) a
    .  ( Generic a
       , Functor (GenericTraverseF tag)
       , GTraverseNonSum tag (Rep a)
    ) => GenericTraverseF tag a
genericTraverseNonSum = to <$> gTraverseNonSum @tag

-- | Generic 'traverse' over a term of sum data type @f a@,
--   where @f@ is set by the @tag@ you pass.
--
-- You must provide a configuration for how to handle constructors.
genericTraverseSum
    :: forall {k} (tag :: k) a pt
    .  ( Generic a
       , Functor (GenericTraverseF tag)
       , GTraverseSum tag (Rep a)
       , GenericTraverseC tag pt
    ) => PfxTagCfg pt -> GenericTraverseF tag a
genericTraverseSum ptc = to <$> gTraverseSum @tag ptc

-- | Construct a prefix tag config using existing 'Eq' and 'Show' instances.
--
-- The user only needs to provide the constructor name parser.
eqShowPfxTagCfg :: (Eq a, Show a) => (String -> a) -> PfxTagCfg a
eqShowPfxTagCfg f = PfxTagCfg
    { pfxTagCfgFromCstr = f
    , pfxTagCfgEq = (==)
    , pfxTagCfgShow = Text.pack . show
    }
