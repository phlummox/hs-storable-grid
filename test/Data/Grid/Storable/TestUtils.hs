
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- |

Some definitions to make data-driven tests more readable.

Otherwise you get tuple of tuples of tuples, ad nauseam,
and it's hard to read.

Also a CRC function.

-}

module Data.Grid.Storable.TestUtils
  where

import            Data.Binary
import            Data.Binary.Put (runPut)
import qualified  Data.ByteString.Lazy as BSL
import qualified  Data.Digest.CRC64 as C64
import qualified  Data.Digest.CRC as C


{-# ANN module ("HLint: ignore Eta reduce"::String) #-}
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

pattern (:-->) :: a -> b -> (a, b)
pattern a :--> b = (a,b)

type a :--> b = (a,b)

type Pair a = (a,a)

crc_anything :: (Foldable t, Binary a) => t a -> C64.CRC64
crc_anything xs =
    C.digest $ BSL.toStrict $ runPut $ foldMap put xs

