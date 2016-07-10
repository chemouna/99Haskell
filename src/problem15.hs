{-# LANGUAGE TemplateHaskell #-}

module NintetyNine.Problem15 where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All

import Data.List
import Data.Foldable
import Control.Applicative

-- Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli xs n = if n < 0
                then error "replicate with negative number"
                else concatMap (\x -> replicate n x) xs

repli2 :: [a] -> Int -> [a]
repli2 xs n
  | n < 0 = error "replicate with negative number"
  | otherwise = concatMap (\x -> replicate n x) xs

repli3 :: [a] -> Int -> [a]
repli3 xs n
  | n < 0 = error "replicate with negative number"
  | otherwise = concatMap (take n . repeat) xs

-- in pointfree style
repli4 :: [a] -> Int -> [a]
repli4 = flip $ concatMap . replicate


-- using list monad
repli5 :: [a] -> Int -> [a]
repli5 xs n = xs >>= replicate n

repli6 :: [a] -> Int -> [a]
repli6 xs n = foldl (\acc x -> acc ++ (replicate n x)) [] xs
  
-- repli6 :: [a] -> Int -> [a]


-- Test
-- hspec
repliSpec :: Spec
repliSpec = do

  describe "Replicate the elements of a list a given number of times on ints." $ do
    it "Decode a run-length encoded list on ints." $ do
        repli [1,2,3] 3 `shouldBe` [1,1,1,2,2,2,3,3,3]
    it "[repli2] Decode a run-length encoded list on ints." $ do
        repli2 [1,2,3] 3 `shouldBe` [1,1,1,2,2,2,3,3,3]
    it "[repli3] Decode a run-length encoded list on ints." $ do
        repli3 [1,2,3] 3 `shouldBe` [1,1,1,2,2,2,3,3,3]
    it "[repli4] Decode a run-length encoded list on ints." $ do
        repli4 [1,2,3] 3 `shouldBe` [1,1,1,2,2,2,3,3,3]
    it "[repli5] Decode a run-length encoded list on ints." $ do
        repli5 [1,2,3] 3 `shouldBe` [1,1,1,2,2,2,3,3,3]
        

    it "Decode a run-length encoded list on a string." $ do
        repli "abc" 3 `shouldBe` "aaabbbccc"
    it "[repli2] Decode a run-length encoded list on a string." $ do
        repli2 "abc" 3 `shouldBe` "aaabbbccc"
    it "[repli3] Decode a run-length encoded list on a string." $ do
        repli3 "abc" 3 `shouldBe` "aaabbbccc"
    it "[repli4] Decode a run-length encoded list on a string." $ do
        repli4 "abc" 3 `shouldBe` "aaabbbccc"
    it "[repli5] Decode a run-length encoded list on a string." $ do
        repli5 "abc" 3 `shouldBe` "aaabbbccc"


-- QuickCheck
-- size should multiply by n in new list
prop_sizeShouldMultiplyByN xs (Positive n) = length (repli xs n) == length xs * n

-- with quickcheck how can i check if n < 0 error is returned

return []
main = $quickCheckAll


