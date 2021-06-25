
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Grid.Storable.Internal
  where

import Control.DeepSeq ( NFData(rnf) )

import            Data.Data      (Data (..))
import            Text.Read      (Read(..), readListPrecDefault )
import            Data.Tuple (swap)
import            Data.Typeable  (Typeable)
import qualified  Data.Vector as V
import qualified  Data.Vector.Generic as G

import            Foreign

import Prelude hiding ( length, null,
                        replicate, (++), concat,
                        head, last,
                        init, tail, take, drop, splitAt, reverse,
                        map, concatMap,
                        zipWith, zipWith3, zip, zip3, unzip, unzip3,
                        filter, takeWhile, dropWhile, span, break,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
#if __GLASGOW_HASKELL__ >= 706
                        foldMap,
#endif
                        all, any, and, or, sum, product, minimum, maximum,
                        scanl, scanl1, scanr, scanr1,
                        enumFromTo, enumFromThenTo,
                        mapM, mapM_, sequence, sequence_ )

import Data.Grid.Storable.Mutable

{-# ANN module ("HLint: ignore Use camelCase"::String) #-}
{-# ANN module ("HLint: ignore Eta reduce"::String) #-}


offset_to_coord :: Integral a => (a, b) -> a -> (a, a)
offset_to_coord (w,_h) n = swap $ divMod n w
{-# INLINEABLE offset_to_coord #-}


-- height must be > 0
coord_to_offset :: Num a => (a, b) -> (a, a) -> a
coord_to_offset (w,_h) (x,y) = y * w + x
{-# INLINEABLE coord_to_offset #-}

-- | internal grid implementation
--
data Grid el a =  Grid {-# UNPACK #-} !(V.Vector a)
                       {-# UNPACK #-} !(ForeignPtr el)
  deriving (Typeable, Data)

liftRnfV :: G.Vector a b => (b -> ()) -> a b -> ()
liftRnfV elemRnf =  G.foldl' (const elemRnf) ()

instance NFData (v el) => NFData (Grid el (v el)) where
  rnf = liftRnfV rnf
  {-# INLINEABLE rnf #-}

instance Show (v el) => Show (Grid el (v el)) where
  showsPrec = G.showsPrec

instance Read (v el) => Read (Grid el (v el)) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault



type instance G.Mutable (Grid el) = MGrid el

instance G.Vector (Grid el) (v el :: *) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze _       = error "you can't possible have got hold of a mutable Grid"
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw _         = error "you can't get thaw Grids"
  {-# INLINE basicLength #-}
  basicLength (Grid vec _) = G.basicLength vec

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j n (Grid vec ptr) = Grid (G.basicUnsafeSlice j n vec) ptr

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Grid vec _ptr) j = G.basicUnsafeIndexM vec j




instance Eq (v el) => Eq ((Grid el) (v el)) where
  {-# INLINE (==) #-}
  (Grid v1 _) == (Grid v2 _) = v1 == v2

  {-# INLINE (/=) #-}
  (Grid v1 _) /= (Grid v2 _) = v1 /= v2

-- See http://trac.haskell.org/vector/ticket/12
instance Ord (v el) => Ord ((Grid el) (v el)) where
  {-# INLINE compare #-}
  compare (Grid v1 _) (Grid v2 _) = compare v1 v2

  {-# INLINE (<) #-}
  (Grid v1 _) < (Grid v2 _) = v1 < v2

  {-# INLINE (<=) #-}
  (Grid v1 _) <= (Grid v2 _) = v1 <= v2

  {-# INLINE (>) #-}
  (Grid v1 _) > (Grid v2 _) = v1 > v2

  {-# INLINE (>=) #-}
  (Grid v1 _) >= (Grid v2 _) = v1 >= v2

-- Length information
-- ------------------

-- | /O(1)/ Yield the length of the vector
length :: (Grid el) (v el) -> Int
{-# INLINE length #-}
length = G.length

-- | /O(1)/ Test whether a vector is empty
null :: (Grid el) (v el) -> Bool
{-# INLINE null #-}
null = G.null

-- Indexing
-- --------

-- | O(1) Indexing
(!) :: (Grid el) (v el) -> Int -> v el
{-# INLINE (!) #-}
(!) = (G.!)

-- | O(1) Safe indexing
(!?) :: (Grid el) (v el) -> Int -> Maybe (v el)
{-# INLINE (!?) #-}
(!?) = (G.!?)

-- | /O(1)/ First element
head :: (Grid el) (v el) -> v el
{-# INLINE head #-}
head = G.head

-- | /O(1)/ Last element
last :: (Grid el) (v el) -> v el
{-# INLINE last #-}
last = G.last

-- | /O(1)/ Unsafe indexing without bounds checking
unsafeIndex :: (Grid el) (v el) -> Int -> v el
{-# INLINE unsafeIndex #-}
unsafeIndex = G.unsafeIndex

-- | /O(1)/ First element without checking if the vector is empty
unsafeHead :: (Grid el) (v el) -> v el
{-# INLINE unsafeHead #-}
unsafeHead = G.unsafeHead

-- | /O(1)/ Last element without checking if the vector is empty
unsafeLast :: (Grid el) (v el) -> v el
{-# INLINE unsafeLast #-}
unsafeLast = G.unsafeLast

-- Extracting subvectors (slicing)
-- -------------------------------

-- | /O(1)/ Yield (v el) slice of the vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Int   -- ^ @i@ starting index
                 -> Int   -- ^ @n@ length
                 -> (Grid el) (v el)
                 -> (Grid el) (v el)
{-# INLINE slice #-}
slice = G.slice

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty.
init :: (Grid el) (v el) -> (Grid el) (v el)
{-# INLINE init #-}
init = G.init


-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty.
tail :: (Grid el) (v el) -> (Grid el) (v el)
{-# INLINE tail #-}
tail = G.tail

-- | /O(1)/ Yield at the first @n@ elements without copying. The vector may
-- contain less than @n@ elements in which case it is returned unchanged.
take :: Int -> (Grid el) (v el) -> (Grid el) (v el)
{-# INLINE take #-}
take = G.take

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector may
-- contain less than @n@ elements in which case an empty vector is returned.
drop :: Int -> (Grid el) (v el) -> (Grid el) (v el)
{-# INLINE drop #-}
drop = G.drop

-- | /O(1)/ Yield the first @n@ elements paired with the remainder without copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@
-- but slightly more efficient.
splitAt :: Int -> (Grid el) (v el) -> ((Grid el) (v el), (Grid el) (v el))
{-# INLINE splitAt #-}
splitAt = G.splitAt


-- | /O(1)/ Yield (v el) slice of the vector without copying. The vector must
-- contain at least @i+n@ elements but this is not checked.
unsafeSlice :: Int   -- ^ @i@ starting index
                       -> Int   -- ^ @n@ length
                       -> (Grid el) (v el)
                       -> (Grid el) (v el)
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty but this is not checked.
unsafeInit :: (Grid el) (v el) -> (Grid el) (v el)
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty but this is not checked.
unsafeTail :: (Grid el) (v el) -> (Grid el) (v el)
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- | /O(1)/ Yield the first @n@ elements without copying. The vector must
-- contain at least @n@ elements but this is not checked.
unsafeTake :: Int -> (Grid el) (v el) -> (Grid el) (v el)
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector
-- must contain at least @n@ elements but this is not checked.
unsafeDrop :: Int -> (Grid el) (v el) -> (Grid el) (v el)
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop


-- Permutations
-- ------------

-- | /O(n)/ Reverse (v el) vector
reverse :: (Grid el) (v el) -> (Grid el) (v el)
{-# INLINE reverse #-}
reverse = G.reverse


-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert (v el) vector to (v el) list
toList :: (Grid el) (v el) -> [v el]
{-# INLINE toList #-}
toList = G.toList



