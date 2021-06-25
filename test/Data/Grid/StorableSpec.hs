{-# LANGUAGE PatternSynonyms #-}

module Data.Grid.StorableSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck


import qualified  Data.ByteString as BS
import qualified  Data.Digest.CRC64 as C64
import qualified  Data.Digest.CRC as C
import qualified  Data.Vector as V
import qualified  Data.Vector.Storable as VS
import            Data.Word

import            Data.Grid.Storable as G


{-# ANN module ("HLint: ignore Use camelCase"::String) #-}
{-# ANN module ("HLint: ignore Redundant do"::String) #-}

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- | no. of els that we'll consider to constitute
-- one dimension of a big-*ish* grid.
--  Not *big*; just big-*ish*.
--  Otherwise tests take too long.
biggish_block_size :: Int
biggish_block_size = 400

spec :: Spec
spec = do
  describe "G.fromVector" $ do
    it "should not corrupt data" $
      property prop_crc_okay
    it "even largeish arrays" $
      property $
        forAll (choose (5, biggish_block_size)) $ \width ->
          forAll (choose (5, biggish_block_size)) $ \height ->
            crc_is_same width height


prop_crc_okay ::
  Positive Int -> Positive Int -> Property
prop_crc_okay (Positive w) (Positive h) =
  crc_is_same w h

-- | generate a random storable vec of Word8's, convert to a Grid,
-- and assert the contents gives same CRC64 (to check for corruption).
--
-- Note that height and width need to be positive.
crc_is_same :: Int -> Int -> Property
crc_is_same width height  = do
  forAll (vector $ height * width) $ \ws -> do
    let
        g :: Grid Word8
        g = G.fromVector width height $ VS.fromList ws

        crcOrig :: C64.CRC64
        crcOrig = C.digest $ BS.pack ws
    crc_tripped <- C.digest . BS.pack . concatMap V.toList <$> G.toList g
    crc_tripped `shouldBe` crcOrig



