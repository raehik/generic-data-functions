# generic-data-functions to-dos
  * add tests (steal some from binrep?)
  * provide a different `traverse` that inspects field names, to eventually
    replicate Gabriella's optparse-generic
  * provide a mixed sum & non-sum generic derivation like my original (& Aeson)
  * provide more versions: binary, cereal, store-style

## Code quality
  * highly granular constraints nice for writing, perhaps worse for type errors?
  * use `Tagged` instead of newtype spam for via?

## Design points
### Stuff constraints into implementation enumerators instead of use sites
Idk. I like how it is, but it's certainly messy.

### GHC 9.10: Use explicit types (`RequiredTypeArguments`)
Will be a while until GHC 9.10 is stable for NixOS, so long-term task.

### Don't keep writing `D1` unwrappers
```haskell
genericFoldMapSum'
    :: forall m a cd f
    .  (Rep a ~ D1 cd f, Generic a, GFoldMapSum' m f)
    => (String -> m)
    -> a -> m
genericFoldMapSum' f = gFoldMapSum' f . unM1 . from
```

It works -- but it's messy, because by moving the `D1` case out of the generic
class, you have another requirement to carry around, which you have to use type
equality on *and* get two new type variables. Lot of pain. Maybe I can write a
closed type family `RepNoD1` which unwraps?

Honestly, I don't think this is worth it. There are more places it would be nice
to refactor to remove copy-paste, but they're very specific, and don't fit super
well with `GHC.Generic`'s concept of solving everything with type classes. Sure,
I could write more structured generic traversals, but now you need carry around
more requirements, and I'm not sure if I'd be giving up performance. (Do I care
enough about that, though...?)
