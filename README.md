[hackage-flatparse]: https://hackage.haskell.org/package/flatparse
[hackage-megaparsec]: https://hackage.haskell.org/package/megaparsec

# generic-data-functions
A small Haskell library providing some funky generics that work over arbitrary
Haskell data types. We handle the sums of products representation; you only need
to pass a handful of definitions. Obtain simple, type-safe generic
serializers/reducers and parsers for almost zero effort.

Well, OK, so really this is a generic binary serialization library. It just so
happens that the generics look like `foldMap` and `traverse`.

## Really?
Kind of. We only handle *sequential concatenation*, being cleanly represented by
builtin type classes. Weirder cases like JSON parsing/serialization have more
going on, and aren't sensibly discussed generically. So you are probably only
going to use this with bytestrings and simple binary formats.

## Why?
It is 2023. There are a number of competing parsing and serialization Haskell
libraries, recently some notable high-performance ones. These are often fairly
experimental. Maybe you want some generics to benchmark some real-world use case
against popular libraries like binary and cereal. But maybe generics aren't
provided. Shucks.

That's a shame, because a binary/cereal-esque generic binary parser or
serializer doesn't have much work to do:

  * traverse the generic sum-of-products tree of the given type left to right
  * defer to the appropriate type class for base cases

Sum types necessitate a little more work. Otherwise, most such parsers and
serializers look fairly comparable to each other. Why are we rewriting this
stuff over and over again?

generic-data-functions provides *reusable generics* which have holes in for your
favorite parsers and serializers. Fill out a few definitions to receive a fresh
new generic instance for your own library, without all the boilerplate.

## Functions
### `foldMap` (L->R)
```haskell
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
```

The user provides the `a -> m` dictionary via a special type class instance.
Constructor fields are mapped and combined left-to-right. Sum representations
are handled by mappending the constructor via a user-supplied `String -> m`
first.

Useful for:

  * simple binary serializers which just concatenate fields together
  * reducing to a numeric value

### `traverse` (L->R)
```haskell
traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
```

The user provides the `f a` dictionary via a special type class instance.
Constructor field actions are run left-to-right.
Sum representations are handled by running a constructor action first (thus
requiring `Monad f`).

Useful for:

  * simple binary parsers which can parse generic `foldMap` output

## Notes
### Orphan instances
This library is designed to work with and around existing libraries and type
classes. Thus, you will likely be dealing in orphans. Instances, that is. That's
life, Jim.

## License
Provided under the MIT license. See `LICENSE` for license text.
