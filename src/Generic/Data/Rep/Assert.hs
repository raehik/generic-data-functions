{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6

{- | Assertions on precise generic data representation.

I like being real picky with my generic code, disallowing misuse at the type
level. However, this makes it less flexible overall, and is a large chunk of the
code I have to write over and over again.

I mainly care about sanity checks, along the lines of "if the generic
representations looks like this, type error out". So they don't make any
term-level changes.

So, instead of hiding these in generic type class instances, I put them in type
family equations. Now we can turn these checks on and off -- and when they're
on, you have to carry around that fact in your types. Fantastic!

Checks are formed as 'Constraint's, where a failure triggers a 'TypeError' and a
success goes to the empty constraint '()' (@'()' :: 'Constraint'@ as well as
'Type').

These checks were always done on the type-level, but they were "inline" with the
rest of the type class. By pulling them out, we *should* be incurring some
compile-time performance penalty (albeit hopefully minor due to the simple
nature of the checks), but making no change to runtime.
-}

module Generic.Data.Rep.Assert where

import GHC.Generics
import GHC.TypeLits ( TypeError )
import Generic.Data.Rep.Error

import Data.Kind

-- | Generic representation assertions, on the constructor level (bits that come
--   after 'D1').
data GCAssert
  = NoEmpty -- ^ Is not an empty type (does not have 0 constructors)
  | NoSum   -- ^ Is not a sum type (has 0 or 1 constructors)
  | NeedSum -- ^ Is     a sum type (has 0 or >2 constructors)

-- | Convert a generic representation constructor-level assertion "label" to the
--   assertion it represents, and make that assertion.
type family ApplyGCAssert x a where
    ApplyGCAssert 'NoEmpty a = GCNoEmpty a
    ApplyGCAssert 'NoSum   a = GCNoSum   a
    ApplyGCAssert 'NeedSum a = GCNeedSum a

-- | Apply a list of generic representation constructor-level assertions.
type ApplyGCAsserts :: [GCAssert] -> (k -> Type) -> Constraint
type family ApplyGCAsserts ls a where
    ApplyGCAsserts '[]       a = ()
    ApplyGCAsserts (l ': ls) a = (ApplyGCAssert l a, ApplyGCAsserts ls a)

type GCNoEmpty :: (k -> Type) -> Constraint
type family GCNoEmpty a where
    GCNoEmpty V1 = TypeError ENoEmpty
    GCNoEmpty a = ()

type GCNoSum :: (k -> Type) -> Constraint
type family GCNoSum a where
    GCNoSum (_ :+: _) = TypeError EUnexpectedSum
    GCNoSum a = ()

type GCNeedSum :: (k -> Type) -> Constraint
type family GCNeedSum a where
    GCNeedSum (C1 _ _) = TypeError EUnexpectedNonSum
    GCNeedSum a = ()
