{-# LANGUAGE TemplateHaskell #-}

module NintetyNine.Problem16 where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All

import Data.List

-- dropEvery "abcdefghik" 3 "abdeghk"

dropEvery :: [a] -> Int -> [a]
dropEvery xs n
  | length xs < n = xs
  | otherwise        = take (n - 1) xs ++ dropEvery (drop n xs) n


dropEvery2 :: [a] -> Int -> [a]
dropEvery2 [] _ = []
dropEvery2 xs n = take (n - 1) xs ++ dropEvery2 (drop n xs) n


-- using flip, zip, map, first , snd, cycle 
dropEvery3 :: [a] -> Int -> [a]
dropEvery3 = 

-- dropEvery x : drop + repeat for every n
-- have a list and delete every element at position multiple of n
-- how to delete an element from a list or create a new list without that element 
   

-- foldl untill find index = n * x

-- scanl where it concatenates every x except if it has index m


-- with splitAt
-- dropEvery :: [a] -> Int -> [a]
-- dropEvery xs n = y ++ (dropEvery ys)
--   where (y:ys) = (splitAt n xs)

-- dropEvery2 :: [a] -> Int -> [a]
-- dropEvery2 xs n = let (y::ys) = (splitAt n xs)
--                       in y ++ (dropEvery2 ys)

-- Test
-- hspec
dropEverySpec :: Spec
dropEverySpec = do
  describe "Drop every N'th element from a list." $ do
    it "Drop every N'th element from a list." $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"
  describe "[With dropEvery2] Drop every N'th element from a list." $ do
    it "Drop every N'th element from a list." $ do
      dropEvery2 "abcdefghik" 3 `shouldBe` "abdeghk"


-- QuickCheck


return []
main = $quickCheckAll


