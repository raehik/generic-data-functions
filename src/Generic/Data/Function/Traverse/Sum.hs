{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# LANGUAGE AllowAmbiguousTypes  #-} -- due to generic typeclass design

module Generic.Data.Function.Traverse.Sum where

import GHC.Generics
import Generic.Data.Function.Util.Generic ( datatypeName', conName' )
import Generic.Data.Function.Traverse.Constructor
  ( GTraverseC(gTraverseC)
  , GenericTraverse(type GenericTraverseF, type GenericTraverseC)
  )
import Generic.Data.Function.Util.Error

import Data.Text ( Text )
import Control.Applicative qualified as Applicative
import Control.Applicative ( Alternative((<|>)) )

{- | Sum type monads that can be generically 'traverse'd.

We use 'Alternative' to handle "which constructor" checking on the term level.
-}
class GenericTraverse tag => GenericTraverseSum tag where
    -- | Try to parse a prefix tag of type 'pt'.
    --
    -- Relevant metadata is provided as arguments.
    genericTraverseSumPfxTagAction
        :: GenericTraverseC tag pt
        => String   -- ^ data type name
        -> GenericTraverseF tag pt

    -- | Parse error due to no constructor matching the parsed prefix tag.
    --
    -- Relevant metadata is provided as arguments.
    genericTraverseSumNoMatchingCstrAction
        :: String   -- ^ data type name
        -> [String] -- ^ non-matching constructor names
        -> Text     -- ^ prefix tag, prettified
        -> GenericTraverseF tag a

-- | How to use a type as a prefix tag in a generic sum type parser.
data PfxTagCfg a = PfxTagCfg
  { pfxTagCfgFromCstr :: String -> a
  -- ^ How to turn a constructor name into a prefix tag.

  , pfxTagCfgEq :: a -> a -> Bool
  -- ^ How to compare prefix tags for equality.
  --
  -- By shoving this into our generic derivation config, we can avoid adding an
  -- insidious 'Eq' constraint. In general, you will want to set this to '(==)'.

  , pfxTagCfgShow :: a -> Text
  -- ^ Make a prefix tag human-readable. 'show' is often appropriate.
  }

class GTraverseSum tag gf where
    gTraverseSum
        :: GenericTraverseC tag pt
        => PfxTagCfg pt -> GenericTraverseF tag (gf p)

instance (GTraverseSumD tag cd gf, Functor (GenericTraverseF tag)
  ) => GTraverseSum tag (D1 cd gf) where
    gTraverseSum ptc = M1 <$> gTraverseSumD @tag @cd ptc

class GTraverseSumD tag cd gf where
    gTraverseSumD
        :: GenericTraverseC tag pt
        => PfxTagCfg pt -> GenericTraverseF tag (gf p)

instance
  ( GenericTraverseSum tag, GTraverseCSum tag cd (l :+: r), Datatype cd
  , Alternative (GenericTraverseF tag)
  , Monad (GenericTraverseF tag)
  ) => GTraverseSumD tag cd (l :+: r) where
    gTraverseSumD = gTraverseSumD' @tag @cd

gTraverseSumD'
    :: forall {p} tag cd gf pt
    .  ( GenericTraverseC tag pt
       , Alternative (GenericTraverseF tag)
       , Monad (GenericTraverseF tag)
       , GenericTraverseSum tag, GTraverseCSum tag cd gf
       , Datatype cd
    ) => PfxTagCfg pt -> GenericTraverseF tag (gf p)
gTraverseSumD' ptc = do
    pt <- genericTraverseSumPfxTagAction @tag cd
    gTraverseCSum @tag @cd ptc pt <|> parseErrorNoMatch pt
  where
    cd = datatypeName' @cd
    parseErrorNoMatch pt =
        genericTraverseSumNoMatchingCstrAction @tag cd testedCstrs ((pfxTagCfgShow ptc) pt)
    testedCstrs = [] -- TODO

instance
  ( GenericTraverseSum tag, GTraverseCSum tag cd (C1 cc gf), Datatype cd
  , Alternative (GenericTraverseF tag)
  , Monad (GenericTraverseF tag)
  ) => GTraverseSumD tag cd (C1 cc gf) where
    gTraverseSumD = gTraverseSumD' @tag @cd

instance GTraverseSumD tag cd V1 where
    gTraverseSumD = error eNoEmpty

class GTraverseCSum tag cd gf where
    gTraverseCSum :: PfxTagCfg pt -> pt -> GenericTraverseF tag (gf p)

-- | Combine constructor options with '(<|>)' ("or").
instance
  ( Alternative (GenericTraverseF tag)
  , GTraverseCSum tag cd l
  , GTraverseCSum tag cd r
  ) => GTraverseCSum tag cd (l :+: r) where
    gTraverseCSum ptc pt = l <|> r
      where
        l = L1 <$> gTraverseCSum @tag @cd ptc pt
        r = R1 <$> gTraverseCSum @tag @cd ptc pt

-- | If the constructor matches the expected prefix tag, then return the action
--   handling that constructor's contents, else return the empty action.
instance
  ( Alternative (GenericTraverseF tag)
  , GTraverseC tag cd cc 0 gf, Constructor cc
  ) => GTraverseCSum tag cd (C1 cc gf) where
    gTraverseCSum ptc pt = do
        if   (pfxTagCfgEq ptc) pt ptCstr
        then M1 <$> gTraverseC @tag @cd @cc @0
        else Applicative.empty
      where
        ptCstr = (pfxTagCfgFromCstr ptc) (conName' @cc)
