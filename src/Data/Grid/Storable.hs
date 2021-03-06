

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

{- |

Use case:

* You've got a block of data you want to work
  with in C or C++ or some other foreign language
* It represents a 2D grid
* Before disposing of it, it would be convenient to treat
  it as a 'V.Vector' of vectors (perhaps for inspecting contents).
* Also, I just wanted to see what's involved in creating
  a Vector instance.

The resulting 'Grid' isn't an especially _good_ instance
of a Vector -- many operations on the 'top-level' vector
are disallowed --
and there's no mutable equivalent, but it seems to
work.

-}

module Data.Grid.Storable
  (
    -- * Vec-of-vec view of storable
    Grid

    -- ** plus helpful types
    --
    -- this is just to make types in repl less awful
  , InternalGrid

    -- * Accessors

    -- ** Length information
  , I.length

    -- ** Indexing
  , (I.!) , (I.!?), I.head, I.last
  , I.unsafeIndex, I.unsafeHead, I.unsafeLast

    -- ** Extracting subvectors (slicing)
  , I.slice, I.init, I.tail, I.take, I.drop

    -- * Construction

    -- ** Initialisation
  , fromVector

    -- * Destruction

  , toList

    -- ** Permutations
  , reverse

  -- * Raw pointers
  , unsafeToForeignPtr0
  )

  where

import Control.Monad
import Control.DeepSeq
import Control.Exception

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

#if !MIN_VERSION_base(4,8,0)
import Data.Functor( (<$>) )
#endif

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

import Foreign ( ForeignPtr, Storable , advancePtr
                , touchForeignPtr , newForeignPtr_
                , withForeignPtr
                )

import System.IO.Unsafe

import qualified Data.Grid.Storable.Internal as I

type InternalGrid = I.Grid

-- | A vector-of-vector view of a block foreign contiguous data
type Grid a = InternalGrid a (VS.Vector a)

{-# ANN module ("HLint: ignore Eta reduce"::String) #-}
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

-- | Convert a storable 'VS.Vector' into vector-of-vectors view.
-- Does a "thaw" of the original 'VS.Vector', which results in a full copy,
-- being made, but has the advantage that
-- the source vector can safely be used again after.
--
-- width and height must be > 0 or user error thrown.
fromVector :: (Storable el) =>
      Int -> Int -> VS.Vector el -> Grid el
fromVector w h vec = unsafePerformIO $  do
    when (w < 1 || h < 1) $
      error $ "width and height must be > 0, were: " <> show (w,h)
    when ( (w * h) > VS.length vec ) $
      error $ "vec not big enough to hold width " <> show w
             <> " x height " <> show h
    mvec <- VS.thaw vec

    let (fptr, _flen) = VSM.unsafeToForeignPtr0 mvec
        offsets = take h [0, 0+w ..]

    rows <- withForeignPtr fptr $ \mptr ->
                  mapM (mk_row . advancePtr mptr) offsets


    let g = I.Grid (V.fromList rows) fptr
    return g
  where
    mk_row ptr = (`VS.unsafeFromForeignPtr0` w) <$> newForeignPtr_ ptr

-- | convert the 'Grid' into a list of (plain, not storable)
-- 'V.Vector's.
toList ::
  (Storable a, NFData a) => Grid a -> IO [V.Vector a]
toList grid = do
  let
      vecs = map VS.convert $ I.toList grid
      fptr = unsafeToForeignPtr0 grid
  vecs <- evaluate $ force vecs
  touchForeignPtr fptr
  return vecs


-- | O(1) Yield the underlying 'ForeignPtr'.
--
-- You can assume the pointer points directly to the data (no offset).
--
-- The data may not be modified through the 'ForeignPtr'.
unsafeToForeignPtr0 :: Grid el -> ForeignPtr el
unsafeToForeignPtr0 (I.Grid _ ptr) = ptr



