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

split4 :: [a] -> Int -> ([a], [a])
split4 xs n = (p1, p2)
  where
    p1 = map snd $ filter ((<= n) . fst) (zip [1..] xs)
    p2 = map snd $ filter ((> n) . fst) (zip [1..] xs)

-- a better more optimized way to write split4
split5 :: [a] -> Int -> ([a], [a])
split5 xs n =
  let y = zip [1..] xs
      p1 = map snd $ filter ((<= n) . fst) y
      p2 = map snd $ filter ((> n) . fst) y
  in (p1, p2)

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
     it "[With split4] Split a list into two." $ do
      split4 "abcdefghik" 3 `shouldBe` ("abc", "defghik")
     it "[With split5] Split a list into two." $ do
      split5 "abcdefghik" 3 `shouldBe` ("abc", "defghik")


-- QuickCheck
-- prop_sizeFirstListShouldBeN (NonEmpty xs) (Positive n) = length (fst (split3 xs n)) == n
-- prop_emptyListZero [] (Positive n)  = split [] n == 0

return []
main = $quickCheckAll
