module Data.Patricia.Word.Conversion where

import           Data.Patricia.Word.Lazy.Internal as Lazy
import           Data.Patricia.Word.Strict.Internal as Strict



-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n)\).
--   Create a lazy 'Lazy.Patricia' tree from a strict one.
--
--   The resulting tree does not share its data representation with the original.
toLazy :: StrictPatricia a -> LazyPatricia a
toLazy t =
  case t of
    Strict.Bin p l r -> Lazy.Bin p (toLazy l) (toLazy r)
    Strict.Tip k a   -> Lazy.Tip k a
    Strict.Nil       -> Lazy.Nil



-- | \(\mathcal{O}(n)\).
--   Create a strict 'Strict.Patricia' tree from a lazy one.
--
--   The resulting tree does not share its data representation with the original.
toStrict :: LazyPatricia a -> StrictPatricia a
toStrict t =
  case t of
    Lazy.Bin p l r -> Strict.Bin p (toStrict l) (toStrict r)
    Lazy.Tip k a   -> Strict.Tip k a
    Lazy.Nil       -> Strict.Nil
