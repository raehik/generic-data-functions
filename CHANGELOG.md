## 0.2.0 (2023-08-04)
* Redesign interface, pushing certain checks out of type classes into top-level
  generic function type signature. It means busier top-level types and more code
  for wrapping them, but it allows for more flexibility and cleans up
  implementation. (And the busyness simply makes explicit the implicit checks
  that were being done before.)

## 0.1.1 (2023-07-20)
  * add work-in-progress store-style generic `foldMap`, encoding constructors by
    their index, at `Generic.Data.Function.FoldMap.SumConsByte`

## 0.1.0 (2023-06-23)
Initial release.

  * extracted from binrep
