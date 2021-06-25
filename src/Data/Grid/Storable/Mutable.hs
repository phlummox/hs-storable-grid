
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ > 710
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
#endif

{- |

A bogus mutable version of 'Data.Grid.Storable.Grid'.

Keeps the typechecker happy, but actually, you can't
permissibly mutate a Grid at all.

You can mutate the rows IN it, but not the Grid.

-}

module Data.Grid.Storable.Mutable
  (
  MGrid
  )
  where

import qualified  Data.Vector.Generic.Mutable as G

{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

data MGrid el s a = MGrid


instance G.MVector (MGrid el) a where
  basicLength _             = error "can't make Grids mutable"
  basicUnsafeSlice _ _ _    = error "can't make Grids mutable"
  basicOverlaps _ _         = error "can't make Grids mutable"

  basicUnsafeNew _          = error "can't make Grids mutable"

#if MIN_VERSION_vector(0,11,0)
  basicInitialize _         = error "can't make Grids mutable"
#endif

  basicUnsafeReplicate _ _  = error "can't make Grids mutable"
  basicUnsafeRead _ _       = error "can't make Grids mutable"
  basicUnsafeWrite _ _ _    = error "can't make Grids mutable"


