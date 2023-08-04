module Generic.Data.Function.Common where

data SumOpts
  = SumOnly           -- ^ Only "proper" sum types are permitted, no singletons.
  | AllowSingletonSum -- ^ Treat a single constructor as a sum type still.
