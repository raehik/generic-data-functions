{-# LANGUAGE UndecidableInstances #-}

module Generic.Type.CstrPath where

import GHC.Generics
import GHC.TypeLits
import Data.Type.Bool ( type If )
import Data.Type.Equality ( type (==) )
import Data.Kind ( type Type )
import Data.Type.List ( type Reverse )

-- | A turn in a binary tree: left node, or right node?
data BTTurn = BTL | BTR

data E
  -- | Got the empty type.
  = EVoid

  -- | No constructor with that requested name was found.
  | ENoSuchCstr

-- | Get the path to a named constructor in a generic type representation.
--
-- The D1 meta must already be stripped.
type GCstrPath :: Symbol -> (k -> Type) -> Either E [BTTurn]
type GCstrPath name gf = GCstrPath' name '[ '( '[], gf)]

-- I leave some stuck cases here for efficiency. They can't happen unless you
-- send weird stuff this way e.g. [V1, ...].
type family GCstrPath' name zippers where
    GCstrPath' name '[ '( '[], V1)] = Left EVoid
    GCstrPath' name '[] = Left ENoSuchCstr
    GCstrPath' name ('(bcs, (C1 (MetaCons name' _ _) _)) : zippers) =
        -- sucks we have to Reverse here, but seems most simple
        If (name == name') (Right (Reverse bcs)) (GCstrPath' name zippers)
    GCstrPath' name ('(bcs, (l :+: r)) : zippers) =
        GCstrPath' name ('((BTL : bcs), l) : '((BTR : bcs), r) : zippers)

type family PrettyE (e :: E) where
    PrettyE EVoid       = Text "got the empty type"
    PrettyE ENoSuchCstr = Text "no matching constructor"
