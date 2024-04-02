{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# LANGUAGE AllowAmbiguousTypes  #-} -- due to generic typeclass design

module Generic.Data.Function.Traverse.Sum where

import GHC.Generics
import Generic.Data.Function.Util.Generic ( datatypeName', conName' )
import Generic.Data.Function.Traverse.Constructor
  ( GTraverseC(gTraverseC)
  , GenericTraverse(type GenericTraverseF, type GenericTraverseC)
  )
import Generic.Data.Rep.Error
import Generic.Data.Function.Common

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

class GTraverseSum (opts :: SumOpts) cd tag gf where
    gTraverseSum
        :: GenericTraverseC tag pt
        => PfxTagCfg pt -> GenericTraverseF tag (gf p)

instance
  ( GenericTraverseSum tag, GTraverseCSum cd tag (l :+: r), Datatype cd
  , Alternative (GenericTraverseF tag)
  , Monad (GenericTraverseF tag)
  ) => GTraverseSum opts cd tag (l :+: r) where
    gTraverseSum = gTraverseSum' @cd @tag

gTraverseSum'
    :: forall {p} cd tag gf pt
    .  ( GenericTraverseC tag pt
       , Alternative (GenericTraverseF tag)
       , Monad (GenericTraverseF tag)
       , GenericTraverseSum tag, GTraverseCSum cd tag gf
       , Datatype cd
    ) => PfxTagCfg pt -> GenericTraverseF tag (gf p)
gTraverseSum' ptc = do
    pt <- genericTraverseSumPfxTagAction @tag cd
    gTraverseCSum @cd @tag ptc pt <|> parseErrorNoMatch pt
  where
    cd = datatypeName' @cd
    parseErrorNoMatch pt =
        genericTraverseSumNoMatchingCstrAction @tag cd testedCstrs ((pfxTagCfgShow ptc) pt)
    testedCstrs = [] -- TODO

instance GTraverseSum 'SumOnly cd tag (C1 cc gf) where
    gTraverseSum = error eNeedSum

instance
  ( GenericTraverseSum tag, GTraverseCSum cd tag (C1 cc gf), Datatype cd
  , Alternative (GenericTraverseF tag)
  , Monad (GenericTraverseF tag)
  ) => GTraverseSum 'AllowSingletonSum cd tag (C1 cc gf) where
    gTraverseSum = gTraverseSum' @cd @tag

instance GTraverseSum opts cd tag V1 where
    gTraverseSum = error eNoEmpty

class GTraverseCSum cd tag gf where
    gTraverseCSum :: PfxTagCfg pt -> pt -> GenericTraverseF tag (gf p)

-- | Combine constructor options with '(<|>)' ("or").
instance
  ( Alternative (GenericTraverseF tag)
  , GTraverseCSum cd tag l
  , GTraverseCSum cd tag r
  ) => GTraverseCSum cd tag (l :+: r) where
    gTraverseCSum ptc pt = l <|> r
      where
        l = L1 <$> gTraverseCSum @cd @tag ptc pt
        r = R1 <$> gTraverseCSum @cd @tag ptc pt

-- | If the constructor matches the expected prefix tag, then return the action
--   handling that constructor's contents, else return the empty action.
instance
  ( Alternative (GenericTraverseF tag)
  , GTraverseC cd cc 0 tag gf, Constructor cc
  ) => GTraverseCSum cd tag (C1 cc gf) where
    gTraverseCSum ptc pt = do
        if   (pfxTagCfgEq ptc) pt ptCstr
        then M1 <$> gTraverseC @cd @cc @0 @tag
        else Applicative.empty
      where
        ptCstr = (pfxTagCfgFromCstr ptc) (conName' @cc)
