{-# LANGUAGE TemplateHaskell #-}

module NinetyNine.Problem17 where

import Data.List
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All

-- Split a list into two parts; the length of the first part is given

-- split "abcdefghik" 3 ("abc", "defghik")
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

split2 :: [a] -> Int -> ([a], [a])
split2 = flip splitAt

split3 :: [a] -> Int -> ([a], [a])
split3 [] _ = ([],[])
split3 l@(x:xs) n | n > 0 = (x:ys, zs)
                  | otherwise = ([],l)
               where (ys,zs) = split3 xs (n-1)

-- Test
-- hspec
splitSpec :: Spec
splitSpec = do
  describe "Split list into two parts where the length of the first one is given." $ do
     it "Split a list into two." $ do
      split "abcdefghik" 3 `shouldBe` ("abc", "defghik")
     it "[With split2] Split a list into two." $ do
      split2 "abcdefghik" 3 `shouldBe` ("abc", "defghik")
     it "[With split3] Split a list into two." $ do
      split3 "abcdefghik" 3 `shouldBe` ("abc", "defghik")


-- QuickCheck
-- prop_sizeFirstListShouldBeN (NonEmpty xs) (Positive n) = length (fst (split3 xs n)) == n
-- prop_emptyListZero [] (Positive n)  = split [] n == 0

return []
main = $quickCheckAll
