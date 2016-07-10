{-# LANGUAGE TemplateHaskell #-}

module NinetyNine.Problem13 where

import Data.List
import NinetyNine.Problem11
import NinetyNine.Problem12

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All

-- Run-length encoding of a list (direct solution).

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs)
  | count == 1 = (Single x) : encodeDirect xs
  | otherwise = (Multiple count x) : encodeDirect rest
  where
    (same, rest) = span (==x) xs
    count = length same + 1

groupToListItem :: Eq a => [a] -> ListItem a
groupToListItem [x] = Single x
groupToListItem (x:xs) = Multiple (length xs + 1) x

encodeDirect2 :: Eq a => [a] -> [ListItem a]
encodeDirect2 xs = map groupToListItem (group xs)



encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
    where
      helper x [] = [(1,x)]
      helper x (y@(a,b):ys)
        | x == b    = (1+a,x):ys
        | otherwise = (1,x):y:ys
 
encodeDirect3 :: Eq a => [a] -> [ListItem a]
encodeDirect3 = map encodeHelper . encode'
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x


-- Test
encodeDirectSpec :: Spec
encodeDirectSpec = do
  describe "encodeDirect" $ do
    it "Decode a run-length encoded listRun-length encoding of a list." $ do
      encodeDirect "aaaabccaadeeee"
      `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

    it "[With encodeDirect2] Decode a run-length encoded listRun-length encoding of a list." $ do
       encodeDirect2 "aaaabccaadeeee"
        `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

    it "[With encodeDirect3] Decode a run-length encoded listRun-length encoding of a list." $ do
       encodeDirect3 "aaaabccaadeeee"
        `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

-- QuickCheck Tests
prop_encodeDirectRoundtrip xs = decodeModified (encodeDirect xs) == xs
prop_encodeDirectSameAsReference xs = encodeDirect xs == encodeModified xs

return []
main = $quickCheckAll


