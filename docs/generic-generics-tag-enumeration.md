# GHC Generics pattern: Generics with holes
GHC's generics are extremely powerful, in part thanks to how much structure a
Haskell data type has. One area in particular which often receives generic
attention is printing & parsing. Given a type class enumerating base instances,
we can write some code using `GHC.Generics` to unwrap the type level
representation of a Haskell data type at compile time, handle any base
instances, and tie them up with sensible product & sum handling.

Note that our base instance type class is hardcoded into our generics. What if
the user didn't like that type class? Perhaps it has the wrong behaviour, or
unwanted instances. In such a case, the user has no recourse other than
reimplementing the generics. `newtype` trickery only gets one so far...

Now, our generics code doesn't _rely_ on any particular base instance behaviour.
It simply needs a function matching a type signature, and an `a :: Constraint`
to check if a given type implements that function at compile time. So we can
create another type class (where the type is uninstantiated), with an associated
type family for the constraint type and a function for the function. Then we
replace references the base hardcoded base instance class with this new
"implementation enumerator" type class.

```
TODO code example
```

We've effectively written generics with a hole for the base case, which the user
must provide an implementation for. But why stop there? We could stuff more
holes into the implementation enumerator type class we wrote earlier to enable
_further_ flexibility. This is how generic `foldMap` came to be:

1. we have `foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m`
2. the generic representation of a Haskell data type is effectively `Foldable`
3. enable specifying `m` (the target monoidal type)
4. enable specifying `a -> m` (the base case)
5. now we just have to write some `<>` glue for products and special handling
   for sums (using constructor names)

Originally, the implementation enumerator type class took `m` as the type
variable. That limits us to a single implementation per target monoid. A newtype
solves that easily, but the interface is already extremely type-heavy, and this
pattern seems more general (e.g. in cases where the target monoid is decided for
you), so I changed it.
