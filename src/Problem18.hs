

module NinetyNine.Problem18 where

import Test.Hspec
import Data.List

--- Extract a slice from a list.

slice :: [a] -> Int -> Int -> [a]
slice xs i k = drop (i-1) (take k xs)


slice2 :: [a] -> Int -> Int -> [a]
slice2 xs i k | i > 0 = take (k - i + 1) $ drop (i-1) xs


slice3 :: [a] -> Int -> Int -> [a]
slice3 xs i k = [x | (x,j) <- zip xs [1..k] , i <= j]


slice4 :: [a] -> Int -> Int -> [a]
slice4 xs a b = fst $ unzip $ filter ((>=a) . snd) $ zip xs [1..b]

sliceSpec :: Spec
sliceSpec = do
  describe "Extract a slice from a list.." $ do
     it "Extract a slice from a list." $ do
       slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"
     it "[With slice2] Extract a slice from a list." $ do
      slice2 ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"
     it "[With slice3] Extract a slice from a list." $ do
      slice3 ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"
     it "[With slice4] Extract a slice from a list." $ do
      slice4 ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"
     
