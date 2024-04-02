{-# LANGUAGE UndecidableInstances #-} -- due to type class design
{-# LANGUAGE AllowAmbiguousTypes  #-}  -- due to type class design

module Generic.Data.Function.Traverse.Constructor where

import GHC.Generics
import GHC.TypeNats ( Natural, KnownNat, type (+) )
import Generic.Data.Function.Util.Generic ( datatypeName', conName', selName'' )
import Generic.Data.Function.Util.TypeNats ( natVal'' )

import Control.Applicative qualified as Applicative
import Control.Applicative ( Alternative(empty) )

import Data.Kind ( type Type, type Constraint )

import Generic.Data.Wrappers ( NoRec0, type ENoRec0, EmptyRec0 )
import GHC.TypeLits ( TypeError )

-- import Data.Monoid

-- | Implementation enumeration type class for generic 'traverse'.
--
-- The type variable is uninstantiated, used purely as a tag.
--
-- Avoid orphan instances by defining custom empty types to use here.
-- See the binrep library on Hackage for an example.
class GenericTraverse tag where
    -- | The target 'Applicative' to 'traverse' to.
    type GenericTraverseF tag :: Type -> Type

    -- | The type class providing the action in 'traverse' for permitted types.
    type GenericTraverseC tag a :: Constraint

    -- | The action in 'traverse' (first argument).
    --
    -- We include data type metadata because this function is useful for monadic
    -- parsers, which can record it in error messages. (We don't do it for
    -- foldMap because it's pure.)
    genericTraverseAction
        :: GenericTraverseC tag a
        => String       {- ^ data type name -}
        -> String       {- ^ constructor name -}
        -> Maybe String {- ^ record name (if present) -}
        -> Natural      {- ^ field index -}
        -> GenericTraverseF tag a

-- | 'traverse' over types with no fields in any constructor.
instance GenericTraverse (NoRec0 (f :: Type -> Type)) where
    type GenericTraverseF (NoRec0 f) = f
    type GenericTraverseC (NoRec0 _) _ = TypeError ENoRec0
    genericTraverseAction = undefined

-- | 'traverse' over types where all fields are replaced with the functor's
--   'empty'.
--
-- Note that one may write a valid instance using a 'Monoid' on @a@s instead.
-- I don't think you should. But I can't explain why.
instance GenericTraverse (EmptyRec0 (f :: Type -> Type)) where
    type GenericTraverseF (EmptyRec0 f) = f
    type GenericTraverseC (EmptyRec0 f) _ = Alternative f
    genericTraverseAction _ _ _ _ = empty

class GTraverseC cd cc (si :: Natural) tag gf where
    gTraverseC :: GenericTraverseF tag (gf p)

instance
  ( Applicative (GenericTraverseF tag)
  , GTraverseC cd cc si                 tag l
  , GTraverseC cd cc (si + ProdArity r) tag r
  ) => GTraverseC cd cc si tag (l :*: r) where
    gTraverseC = Applicative.liftA2 (:*:)
                   (gTraverseC @cd @cc @si                 @tag)
                   (gTraverseC @cd @cc @(si + ProdArity r) @tag)

instance
  ( GenericTraverse tag, GenericTraverseC tag a
  , Functor (GenericTraverseF tag)
  , KnownNat si, Selector cs, Constructor cc, Datatype cd
  ) => GTraverseC cd cc si tag (S1 cs (Rec0 a)) where
    gTraverseC = (M1 . K1) <$> genericTraverseAction @tag cd cc cs si
      where
        cs = selName'' @cs
        cd = datatypeName' @cd
        cc = conName' @cc
        si = natVal'' @si

instance Applicative (GenericTraverseF tag) => GTraverseC cd cc 0 tag U1 where
    gTraverseC = pure U1

type family ProdArity (f :: Type -> Type) :: Natural where
    ProdArity (S1 c f)  = 1
    ProdArity (l :*: r) = ProdArity l + ProdArity r
