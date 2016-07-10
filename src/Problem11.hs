{-# LANGUAGE TemplateHaskell #-}

module NinetyNine.Problem11 where

import Data.List

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All

-- import NinetyNine.Problem9
import NinetyNine.Problem10

-- Modify the result of problem 10 in such a way that if an element has no duplicates
-- it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

data ListItem a = Single a | Multiple Int a
    deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeFn . encode
    where
        encodeFn (1,x) = Single x
        encodeFn (n,x) = Multiple n x


-- Test
encodeModifiedSpec :: Spec
encodeModifiedSpec = do
  describe "encodeModified" $ do
    it "Modified run-length encoding. of a list." $ do
      encodeModified "aaaabccaadeeee"
        `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']

-- quickcheck
-- size should be same as encode
prop_encodeModifiedSameSize xs = length (encode xs) == length (encodeModified xs)

return []
main = $quickCheckAll




