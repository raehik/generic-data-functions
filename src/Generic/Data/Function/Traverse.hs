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

import Generic.Data.Rep.Assert
import Generic.Data.Function.Traverse.NonSum
import Generic.Data.Function.Traverse.Sum
import Generic.Data.Function.Traverse.Constructor

import Data.Text qualified as Text

-- | Generic 'traverse' over a term of non-sum data type @f a@.
genericTraverseNonSum
    :: forall {cd} {gf} asserts tag a
    .  ( Generic a, Rep a ~ D1 cd gf
       , GTraverseNonSum cd tag gf
       , ApplyGCAsserts asserts tag
       , Functor (GenericTraverseF tag))
    => GenericTraverseF tag a
genericTraverseNonSum = (to . M1) <$> gTraverseNonSum @cd @tag

-- | Generic 'traverse' over a term of sum data type @f a@.
--
-- You must provide a configuration for how to handle constructors.
genericTraverseSum
    :: forall {cd} {gf} opts asserts tag a pt
    .  ( Generic a, Rep a ~ D1 cd gf
       , GTraverseSum opts cd tag gf
       , ApplyGCAsserts asserts tag
       , GenericTraverseC tag pt, Functor (GenericTraverseF tag))
    => PfxTagCfg pt
    -> GenericTraverseF tag a
genericTraverseSum ptc = (to . M1) <$> gTraverseSum @opts @cd @tag ptc

-- | Construct a prefix tag config using existing 'Eq' and 'Show' instances.
--
-- The user only needs to provide the constructor name parser.
eqShowPfxTagCfg :: (Eq a, Show a) => (String -> a) -> PfxTagCfg a
eqShowPfxTagCfg f = PfxTagCfg
    { pfxTagCfgFromCstr = f
    , pfxTagCfgEq = (==)
    , pfxTagCfgShow = Text.pack . show
    }
