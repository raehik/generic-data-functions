# generic-data-functions to-dos
  * add tests (steal some from binrep?)
  * provide a different `traverse` that inspects field names, to eventually
    replicate Gabriella's optparse-generic
  * provide more versions: binary, cereal, store-style

## Code quality
  * highly granular constraints nice for writing, perhaps worse for type errors?
  * use `Tagged` instead of newtype spam for via?

## Design points
### Stuff constraints into implementation enumerators instead of use sites
Idk. I like how it is, but it's certainly messy.

### GHC 9.10: Use explicit types (`RequiredTypeArguments`)
Will be a while until GHC 9.10 is stable for NixOS, so long-term task.
