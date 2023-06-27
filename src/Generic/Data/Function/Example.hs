module Generic.Data.Function.Example where

import GHC.Generics ( Generic )
import Generic.Data.Function.FoldMap.Constructor ( GenericFoldMap(..) )

data D a = D1 a | D2 a a deriving stock (Generic, Show)

newtype Showly a = Showly { unShowly :: a }
    deriving Show via a
    deriving (Semigroup, Monoid) via a

instance GenericFoldMap (Showly String) where
    type GenericFoldMapC (Showly String) a = Show a
    genericFoldMapF = Showly . show
