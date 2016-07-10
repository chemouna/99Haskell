module NinetyNine.Problem9 where

import Control.Monad
import Data.List

import Test.Hspec

-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
-- to find a sol try with folds, foldmap, bind, reverse bind, multiple guards with where

pack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest
pack [] = []

-- Test
packSpec :: Spec
packSpec = do
  describe "pack" $ do
    it "Pack consecutive duplicates of list elements into sublists." $ do
      pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
           `shouldBe` ["aaaa","b","cc","aa","d","eeee"]

