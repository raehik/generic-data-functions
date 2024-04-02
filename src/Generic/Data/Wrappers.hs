-- | Wrappers for "free" generics, where the base case is handled for you.

module Generic.Data.Wrappers where

import GHC.TypeLits ( ErrorMessage(Text) )

-- | Free generic wrapper where any field emits a type error.
--
-- Useful for generic functions on void or enum types.
data NoRec0 (a :: k)

type ENoRec0 =
    'Text "Cannot use generic function on NoRec0-wrapped type containing fields"

-- | Free generic wrapper where every field does "nothing" (e.g. 'mempty'.)
--
-- Maybe useful for testing?
data EmptyRec0 (a :: k)
