{-# LANGUAGE AllowAmbiguousTypes #-}

{- | 'foldMap' for generic data types.

'foldMap' can be considered a two-step process:

  * map every element @a@ of a @t a@ (where @'Foldable' t@) to some @'Monoid' m@
  * combine elements using '(<>)'

Applying this to generic data types:

  * map every field of a constructor to some @'Monoid' m@
  * combine elements using '(<>)'

Field mappings are handled using a per-monoid type class. You need a monoid @m@
with an associated type class which has a function @a -> m@. Write a
'GenericFoldMap' instance for your monoid which points to your type class. If a
field type doesn't have a matching instance, the generic instance emits a type
error.

Sum types (with multiple constructors) are handled by '(<>)'-ing the constructor
with its contents (in that order). You must provide a @String -> m@ function for
mapping constructor names. If you need custom sum type handling, you may write
your own and still leverage the individual constructor generics.

This function can provide generic support for simple fold-y operations like
serialization.
-}

module Generic.Data.Function.FoldMap
  ( GenericFoldMap(..)
  , genericFoldMapNonSum, GFoldMapNonSum
  , genericFoldMapSum,    GFoldMapSum
  , genericFoldMapSumConsByte,    GFoldMapSumConsByte
  ) where

import GHC.Generics

import Generic.Data.Function.FoldMap.NonSum
import Generic.Data.Function.FoldMap.Sum
import Generic.Data.Function.FoldMap.Constructor
import Generic.Data.Function.FoldMap.SumConsByte
import Data.Word ( Word8 )

-- | Generic 'foldMap' over a term of non-sum data type @a@.
--
-- @a@ must have exactly one constructor.
genericFoldMapNonSum
    :: forall {k} (tag :: k) a
    .  ( Generic a, GFoldMapNonSum tag (Rep a)
    ) => a -> GenericFoldMapM tag
genericFoldMapNonSum = gFoldMapNonSum @tag . from

-- | Generic 'foldMap' over a term of sum data type @a@.
--
-- You must provide a function for mapping constructor names to monoidal values.
--
-- This is the most generic option, but depending on your string manipulation
-- may be slower.
genericFoldMapSum
    :: forall {k} (tag :: k) a
    .  ( Generic a, GFoldMapSum tag (Rep a)
    ) => (String -> GenericFoldMapM tag)
    -> a -> GenericFoldMapM tag
genericFoldMapSum f = gFoldMapSum @tag f . from

-- | Generic 'foldMap' over a term of sum data type @a@ where constructors are
-- mapped to their index (distance from first/leftmost constructor)
--
-- @a@ must have at least two constructors.
--
-- You must provide a function for mapping bytes to monoidal values.
--
-- This should be fairly fast, but sadly I think it's slower than the generics
-- in store and binary/cereal libraries.
genericFoldMapSumConsByte
    :: forall tag a
    .  (Generic a, GFoldMapSumConsByte tag (Rep a))
    => (Word8 -> GenericFoldMapM tag)
    -> a -> GenericFoldMapM tag
genericFoldMapSumConsByte f = gFoldMapSumConsByte @tag f . from
