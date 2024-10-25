{-# LANGUAGE BangPatterns
           , RankNTypes #-}

module Data.RadixNTree.Word8.Key
  ( Tsil (..)
  , YtpmeNon (..)
  , Build (..)

  , buildBytes0

  , buildByteString0
  , buildShortByteString0

  , unsafeBuildText0

  , Build1 (..)

  , buildBytes1

  , buildByteString1
  , buildShortByteString1

  , unsafeBuildText1

  , Feed (..)
  , feedBytes0

  , feedByteString0
  , feedShortByteString0
  , feedLazyByteString0

  , feedText0
  , feedLazyText0

  , Feed1 (..)
  , feedBytes1

  , unsafeFeedByteString1
  , unsafeFeedShortByteString1
  , unsafeFeedLazyByteString1

  , unsafeFeedText1
  , unsafeFeedLazyText1
  ) where

import           Data.ByteArray.NonEmpty

import           Control.Monad.ST
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as Strict (ByteString (..), unsafeCreate)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy.Internal as LazyBS (ByteString (..))
import           Data.ByteString.Short.Internal (ShortByteString (..))
import           Data.ByteString.Unsafe
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Primitive.ByteArray
import qualified Data.Text.Array as Array
import qualified Data.Text.Internal as Strict (Text (..))
import qualified Data.Text.Internal.Lazy as LazyText (Text (..))
import qualified Data.Text.Lazy as Lazy (Text)
import           Data.Word
import           Foreign.Ptr



-- | Snoc-list.
data Tsil a = Lin
            | Snoc (Tsil a) a

-- | Snoc-list with a guaranteed element at the back.
data YtpmeNon a = Tsil a :/ a

-- | Key as stored in the radix tree.
newtype Build = Build
                  -- | List of memory chunks that constitute the key.
                  --
                  --   The first chunk is at the bottom of the list.
                  (Tsil ByteArray)

instance Show Build where
  showsPrec d = showsPrec d . buildBytes0

-- | Non-empty key as stored in the radix tree.
newtype Build1 = Build1
                   -- | List of memory chunks that constitute the key.
                   --
                   --   The first chunk is at the bottom of the list.
                   (YtpmeNon ByteArray)

instance Show Build1 where
  showsPrec d xs = let ~(y :| ys) = buildBytes1 xs
                   in showsPrec d (y:ys)

buildBytes0 :: Build -> [Word8]
buildBytes0 (Build xs) = go [] xs
  where
    go acc as =
      case as of
        Snoc bs a -> go (Data.ByteArray.NonEmpty.toList a <> acc) bs
        Lin       -> acc

buildBytes1 :: Build1 -> NonEmpty Word8
buildBytes1 (Build1 (xs :/ x)) = go (toNonEmpty x) xs
  where
    go acc as =
      case as of
        Snoc bs a -> go (toNonEmpty a <> acc) bs
        Lin       -> acc



sizeofBuild0 :: Build -> Int
sizeofBuild0 (Build xs) = go xs
  where
    go as =
      case as of
        Snoc bs arr -> sizeofByteArray arr + go bs
        Lin         -> 0

sizeofBuild1 :: Build1 -> Int
sizeofBuild1 (Build1 (xs :/ arr)) = sizeofByteArray arr + sizeofBuild0 (Build xs)

writePtr :: Ptr Word8 -> Int -> Build -> IO ()
writePtr ptr off0 (Build xs) = go off0 xs
  where
    go off as =
      case as of
        Snoc bs arr -> do
          let off' = off - sizeofByteArray arr
          copyByteArrayToAddr (plusPtr ptr off') arr 0 (sizeofByteArray arr)
          go off' bs

        Lin         -> pure ()

writePtr1 :: Ptr Word8 -> Int -> Build1 -> IO ()
writePtr1 ptr off (Build1 (xs :/ arr)) = do
  let off' = off - sizeofByteArray arr
  copyByteArrayToAddr (plusPtr ptr off') arr 0 (sizeofByteArray arr)
  writePtr ptr off' (Build xs)



buildByteString0 :: Build -> Strict.ByteString
buildByteString0 xs =
  let len = sizeofBuild0 xs
  in Strict.unsafeCreate len (\ptr -> writePtr ptr len xs)

buildByteString1 :: Build1 -> Strict.ByteString
buildByteString1 xs =
  let len = sizeofBuild1 xs
  in Strict.unsafeCreate len (\ptr -> writePtr1 ptr len xs)



writeArr :: MutableByteArray s -> Int -> Build -> ST s ()
writeArr marr off0 (Build xs) = go off0 xs
  where
    go off as =
      case as of
        Snoc bs arr -> do
          let off' = off - sizeofByteArray arr
          copyByteArray marr off' arr 0 (sizeofByteArray arr)
          go off' bs

        Lin         -> pure ()

writeArr1 :: MutableByteArray s -> Int -> Build1 -> ST s ()
writeArr1 marr off (Build1 (xs :/ arr)) = do
  let off' = off - sizeofByteArray arr
  copyByteArray marr off' arr 0 (sizeofByteArray arr)
  writeArr marr off' (Build xs)



{-# INLINE buildShortByteString0 #-}
buildShortByteString0 :: Build -> ShortByteString
buildShortByteString0 xs =
  runST $ do
    let len = sizeofBuild0 xs
    marr <- newByteArray len
    writeArr marr len xs
    ByteArray arr <- unsafeFreezeByteArray marr
    pure $ SBS arr

{-# INLINE buildShortByteString1 #-}
buildShortByteString1 :: Build1 -> ShortByteString
buildShortByteString1 xs =
  runST $ do
    let len = sizeofBuild1 xs
    marr <- newByteArray len
    writeArr1 marr len xs
    ByteArray arr <- unsafeFreezeByteArray marr
    pure $ SBS arr

{-# INLINE unsafeBuildText0 #-}
unsafeBuildText0 :: Build -> Strict.Text
unsafeBuildText0 xs =
  runST $ do
    let len = sizeofBuild0 xs
    marr <- newByteArray len
    writeArr marr len xs
    ByteArray arr <- unsafeFreezeByteArray marr
    pure $ Strict.Text (Array.ByteArray arr) 0 len

{-# INLINE unsafeBuildText1 #-}
unsafeBuildText1 :: Build1 -> Strict.Text
unsafeBuildText1 xs =
  runST $ do
    let len = sizeofBuild1 xs
    marr <- newByteArray len
    writeArr1 marr len xs
    ByteArray arr <- unsafeFreezeByteArray marr
    pure $ Strict.Text (Array.ByteArray arr) 0 len



-- | Key as a sequence of individual bytes.
newtype Feed = Feed
                 -- | @destroy@ part of the @destroy/unfoldr@ rule.
                 (forall a. (forall x. (x -> Step Word8 x) -> x -> a) -> a)

{-# INLINE vomit #-}
vomit :: (x -> Step a x) -> x -> [a]
vomit step = go
  where
    go s =
      case step s of
        More w ws -> w : go ws
        Done      -> []

instance Show Feed where
  showsPrec d (Feed f) = showsPrec d $ f vomit

noFeed :: Feed
noFeed = Feed $ \f -> f (\_ -> Done) ()

{-# INLINE feedBytes0 #-}
feedBytes0 :: [Word8] -> Feed
feedBytes0 ws0 = Feed $ \f -> f go ws0
  where
    go (w:ws) = More w ws
    go []     = Done



-- | Key as a non-empty sequence of individual bytes.
data Feed1 = Feed1
               -- | First byte of the key.
               {-# UNPACK #-} !Word8

               -- | @destroy@ part of the @destroy/unfoldr@ rule.
               (forall a. (forall x. (x -> Step Word8 x) -> x -> a) -> a)

instance Show Feed1 where
  showsPrec d (Feed1 w0 f) = showsPrec d $ w0 :| f vomit

{-# INLINE feedBytes1 #-}
feedBytes1 :: NonEmpty Word8 -> Feed1
feedBytes1 (w0 :| ws) =
  let Feed f = feedBytes0 ws
  in Feed1 w0 f




stepByteString :: Strict.ByteString -> Int -> Step Word8 Int
stepByteString bs = go
  where
    go n =
      if n >= BS.length bs
        then Done
        else let !n' = n + 1
             in More (unsafeIndex bs n) n'

{-# INLINE feedByteString0 #-}
feedByteString0 :: Strict.ByteString -> Feed
feedByteString0 bs = Feed $ \f -> f (stepByteString bs) 0

{-# INLINE unsafeFeedByteString1 #-}
unsafeFeedByteString1 :: Strict.ByteString -> Feed1
unsafeFeedByteString1 bs = Feed1 (unsafeIndex bs 0) (\f -> f (stepByteString bs) 1)



stepByteArray :: ByteArray -> Int -> Int -> Step Word8 Int
stepByteArray arr len = go
  where
    go n =
      if n >= len
        then Done
        else let !n' = n + 1
             in More (indexByteArray arr n) n'

{-# INLINE feedShortByteString0 #-}
feedShortByteString0 :: ShortByteString -> Feed
feedShortByteString0 (SBS arr) =
  Feed $ \f ->
    f (stepByteArray (ByteArray arr) $ sizeofByteArray (ByteArray arr)) 0

{-# INLINE unsafeFeedShortByteString1 #-}
unsafeFeedShortByteString1 :: ShortByteString -> Feed1
unsafeFeedShortByteString1 (SBS arr) =
  Feed1 (indexByteArray (ByteArray arr) 0) $ \f ->
    f (stepByteArray (ByteArray arr) $ sizeofByteArray (ByteArray arr)) 1



{-# INLINE feedText0 #-}
feedText0 :: Strict.Text -> Feed
feedText0 (Strict.Text (Array.ByteArray arr) n len) =
  Feed $ \f ->
    f (stepByteArray (ByteArray arr) len) n

{-# INLINE unsafeFeedText1 #-}
unsafeFeedText1 :: Strict.Text -> Feed1
unsafeFeedText1 (Strict.Text (Array.ByteArray arr) n len) =
  Feed1 (indexByteArray (ByteArray arr) n) $ \f ->
    let !n' = n + 1
    in f (stepByteArray (ByteArray arr) len) n'



data CarryBS = CarryBS
                 {-# UNPACK #-} !Int
                 !Strict.ByteString
                 !Lazy.ByteString

stepLazyByteString :: CarryBS -> Step Word8 CarryBS
stepLazyByteString (CarryBS n bs lbs) =
  if n >= BS.length bs
    then case lbs of
           LazyBS.Chunk bs' lbs' -> stepLazyByteString (CarryBS 0 bs' lbs')
           LazyBS.Empty          -> Done

    else let !n' = n + 1
         in More (unsafeIndex bs n) (CarryBS n' bs lbs)

{-# INLINE feedLazyByteString0 #-}
feedLazyByteString0 :: Lazy.ByteString -> Feed
feedLazyByteString0 b =
  case b of
    LazyBS.Empty        -> noFeed
    LazyBS.Chunk bs lbs -> Feed $ \f -> f stepLazyByteString (CarryBS 0 bs lbs)

{-# INLINE unsafeFeedLazyByteString1 #-}
unsafeFeedLazyByteString1 :: Strict.ByteString -> Lazy.ByteString -> Feed1
unsafeFeedLazyByteString1 bs lbs =
  Feed1 (unsafeIndex bs 0) $ \f ->
    f stepLazyByteString (CarryBS 1 bs lbs)



data CarryTxt = CarryTxt
                  {-# UNPACK #-} !Int
                  {-# UNPACK #-} !Int
                  !ByteArray
                  !Lazy.Text

stepLazyText :: CarryTxt -> Step Word8 CarryTxt
stepLazyText (CarryTxt n len arr t) =
  if n >= len
    then case t of
           LazyText.Chunk (Strict.Text (Array.ByteArray arr') n' len') t' ->
             stepLazyText (CarryTxt n' len' (ByteArray arr') t')

           LazyText.Empty -> Done

    else let !n' = n + 1
         in More (indexByteArray arr n) (CarryTxt n' len arr t)

{-# INLINE feedLazyText0 #-}
feedLazyText0 :: Lazy.Text -> Feed
feedLazyText0 t =
  case t of
    LazyText.Empty                                                -> noFeed
    LazyText.Chunk (Strict.Text (Array.ByteArray arr) n len) ltxt ->
      Feed $ \f -> f stepLazyText (CarryTxt n len (ByteArray arr) ltxt)

{-# INLINE unsafeFeedLazyText1 #-}
unsafeFeedLazyText1 :: Strict.Text -> Lazy.Text -> Feed1
unsafeFeedLazyText1 (Strict.Text (Array.ByteArray arr) n len) ltxt =
  Feed1 (indexByteArray (ByteArray arr) n) $ \f ->
    let !n' = n + 1
    in f stepLazyText (CarryTxt n' len (ByteArray arr) ltxt)
