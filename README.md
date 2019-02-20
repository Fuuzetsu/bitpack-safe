# bitpack-safe

Type-safe packing and extraction of bit collections.

This package tries to alleviate the error prone and ugly process of
packing bits of multiple values into a larger container that will
hold onto them. There are many space, performance or API
constraints due to which you may want to do this.

Further, this package tries to do some _efficiently_. There are
some optimisations that are too error prone or ugly to do manually,
or even operations that you'd somewhat expect the compiler to do
but in reality, GHC does not perform. A couple of examples are:

* When we only want to compare some (or all) fields of packed value,
  it is not necessary to unpack the fields one by one: instead you can
  pack values you're using comparing against and do a single
  comparison. This relies on fields not overlapping and getting all
  the offsets and values right. If you have 30 fields (say, Bools)
  that you want to check for all being in various states of being set,
  it's easy and efficient.

* When we are decoding fields of certain specific sizes (such as 8,
  16, 32, 64), we can save on some operations: there is no need to
  mask out other bits when we can simply narrow the underlying
  container. See <https://ghc.haskell.org/trac/ghc/ticket/16402>:
  in this library, this issue is specifically side-stepped so you
  can actually get good code generated.

This library tries to a fair degree to generate better code where
easily possible. Where it does not perform optimisations, it should
generate no worse code than what you may write by hand without any
clever, manual optimisations.

# TODO for 0.1.0.0

* Add some tests.

## Possible future work

* More useful operations. For example, if we are storing a numeral on
  the left edge, we can add to it in place instead of unpacking,
  shifting, adding, clearing previous mask and ORing. GHC can't notice
  this by itself so we have to provide operations for it.

* Currently the underlying container is Word64: this is not so great
  if you want to pack 8 booleans into a Word8. My first initial
  usecase was 64-bit values but I'm open to extending the library to
  other container types.

* If the above is added, some sort of support for nesting types should
  be added. It's much easier to pack 8 collections of 8 bools than 64
  bools.

* I suspect type errors are not great. There are a few custom messages
  but I don't know how they'll look like once the user starts messing
  with things. For now it's expected that the `Packed` values will be
  fairly monomorphic.
