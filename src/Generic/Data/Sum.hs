{-# LANGUAGE TemplateHaskell #-} -- due to GHC bug

module Generic.Data.Sum where

import Data.Kind ( type Constraint )
import GHC.TypeLits ( type Symbol, KnownSymbol )
import GHC.TypeError ( ErrorMessage(..), type TypeError )

{- TODO usage notes because this is somewhat fragile
* parse result kind can't be in same class due to "type constructor cannot be
  used here (it is defined and used in the same recursive group)"
* got to add a TH splice between parse result kind and user due to GHC bug
-}

class CstrParse' tag where
    -- | Result kind of the type-level constructor name parser.
    type CstrTy tag :: k

class CstrParse' tag => CstrParse tag where
    -- | Constructor name parser.
    --
    -- The Symparsec library generates type families that look like this.
    type ParseCstr tag (sym :: Symbol) :: Either ErrorMessage (CstrTy tag)

    -- | Constraint enabling reification of the parsed type-level constructor
    --   name.
    --
    -- For example, you might reify @'(a, b) :: (Symbol, Symbol)@ with
    -- @(KnownSymbol a, KnownSymbol b)@.
    type ParseCstrC tag (x :: CstrTy tag) :: Constraint

-- | Unwrap a generic constructor parse result. Emits a 'TypeError' on parse
--   failure.
type ForceGCParse :: Symbol -> Symbol -> Either ErrorMessage k -> k
type family ForceGCParse dtName cstr a where
    ForceGCParse _      _    (Right a) = a
    ForceGCParse dtName cstr (Left  e) = TypeError
      ( Text "error while parsing "
        :<>: Text dtName :<>: Text "." :<>: Text cstr :<>: Text ":"
        :$$: e
      )

-- | Return the constructor unparsed.
data Raw

instance CstrParse' Raw where type CstrTy Raw = Symbol
$(pure [])
instance CstrParse  Raw where
    type ParseCstr  Raw str = Right str
    type ParseCstrC Raw str = KnownSymbol str
