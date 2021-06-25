
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE CPP #-}

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
  basicInitialize _         = error "can't make Grids mutable"
  basicUnsafeReplicate _ _  = error "can't make Grids mutable"
  basicUnsafeRead _ _       = error "can't make Grids mutable"
  basicUnsafeWrite _ _ _    = error "can't make Grids mutable"


