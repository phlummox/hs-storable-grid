{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}


module Data.Grid.Storable.InternalSpec (main, spec) where

import Control.Monad

import Test.Hspec
import Test.QuickCheck

import Data.Grid.Storable.Internal
import Data.Grid.Storable.TestUtils

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let table :: [ ( Pair Int, Pair Int ) :--> Int ]
      table = [
           ((7,5), (0,0))  :-->  0
         , ((7,5), (1,1))  :-->  8
         , ((7,5), (6,4))  :-->  34
         ]
  describe "coord_to_offset" $
    it "should give specified results" $ 
      forM_ table $ \( ((w,h), (x,y)) :--> expec_res ) ->
        let actual = coord_to_offset (w,h) (x,y)
        in actual `shouldBe` expec_res
  -- now go in reverse
  describe "offset_to_coord" $
      it "ditto" $
      forM_ table $ \( ((w,h), (x,y)) :--> n ) ->
        let actual = offset_to_coord (w,h) n
        in actual `shouldBe` (x,y)
  describe "and the two of them " $
    it "should be inverses" $
      property prop_offsets_roundtrip

prop_offsets_roundtrip ::
  (Positive Int, Positive Int) -> Property
prop_offsets_roundtrip (Positive w, Positive h) =
  forAll (choose (0,h-1)) $ \y ->
    forAll (choose (0,w-1)) $ \x ->
      let n = coord_to_offset (w,h) (x,y)
          res = offset_to_coord (w,h) n
      in res `shouldBe` (x,y)


