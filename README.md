# radix-tree [![Hackage](http://img.shields.io/hackage/v/radix-tree.svg)](https://hackage.haskell.org/package/radix-tree)


A Haskell library for [radix trees](https://en.wikipedia.org/wiki/Radix_tree).


> [!IMPORTANT]
>
> "strict" and "lazy" interfaces within
> [`containers`](https://hackage.haskell.org/package/containers) and
> [`unordered-containers`](https://hackage.haskell.org/package/containers)
> refer to how new values are inserted into the data structures: "strict" means
> they're additionally evaluated to WHNF, "lazy" means they aren't.
> The data structures themselves are spine-strict in either case.
>
> Within this library "strict" and "lazy" refer to spine-strict and spine-lazy
> variants of a given data structure respectively. Evaluating the values before inserting
> them is directly assumed to be user's responsibility.


Featuring, in order of complexity:

- `Data.Patricia.Word.*`: a
  [PATRICIA tree](https://en.wikipedia.org/w/index.php?title=Radix_tree&oldid=1196786955#Variants).

  The spine-strict variant is effectively identical to
  [`containers#IntMap`](https://hackage.haskell.org/package/containers-0.7/docs/Data-IntMap-Strict.html#t:IntMap).


- `Data.Zebra.Word`: a space-partitioning tree based on a PATRICIA tree.

  Similar to a
  [`containers#IntSet`](https://hackage.haskell.org/package/containers-0.7/docs/Data-IntSet.html#t:IntSet),
  a `Zebra` stores keys more optimally than a naive `StrictPatricia ()`.
  The approaches are however different:

  - An `IntSet` stores packs of 32/64
    (depending on target platform integer size) adjacent bits together.
    Fully identical feature-wise to regular `IntMap`s otherwise.

  - A `Zebra` partitions the space into black and white zones, effectively storing
    intervals of colors. This allows for fast range fills (see `fillRange`) as well as
    fast lookups of the next key of a particular color (see `lookupL` and `lookupR`).

  Due to the way it is constructed a `Zebra` cannot be spine-lazy.


- `Data.RadixTree.Word8.*`: a radix tree.

  A general-purpose dictionary type. Asymptotically faster than
  [`containers#Map`](https://hackage.haskell.org/package/containers-0.7/docs/Data-Map-Strict.html)
  (common key prefixes are only scrutinized once) and far more powerful than
  [`unordered-containers#HashMap`](https://hackage.haskell.org/package/unordered-containers-0.2.20/docs/Data-HashMap-Strict.html#t:HashMap)
  (no hash collisions, lookups can fail early, tree can be spine-lazy).

  Note that unlike most dictionaries a `RadixTree` does not have a concrete key type
  and instead uses two key representations: `Feed` (a key broken down to individual bytes)
  and `Build` (a key reconstructed from chunks as they are found within the tree).
  It is thus perfectly legal to mix together different key types, as long as they make
  sense (e.g. a tree of ASCII keys can be treated as a tree of UTF-8 ones at no cost).


- `Data.Radix1Tree.Word8.*`: a radix tree that cannot store anything at the empty key.

  Exists as a consequence of internal implementation and is convenient for certain
  formats where empty keys are impossible (such as commandline options and INI files).
  Fully identical feature-wise to regular `RadixTree`s otherwise.
