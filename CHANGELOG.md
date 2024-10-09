## 1.1.0.0 -- October 2024

* Added `Zipper` modules for non-empty radix trees;

* Removed `Cursor`s from radix tree modules.
  Their functionality is subsumed by the appropriate `Zipper` modules.

* Added `Pointer` modules for strict radix trees;

## 1.0.0.2 -- September 2024

* Fixed `Data.Zebra.Word.fillRange`.
  Previously it produced malformed trees in certain cases.

## 1.0.0.1 -- May 2024

* Radix tree performance tweaks

## 1.0.0.0 -- April 2024

* Initial rewrite
