module NinetyNine.Problem28 where

import Test.Hspec
import Data.List
import Data.Ord

-- Sorting a list of lists according to length of sublists
--  ["Abc","de","fgh","de","ijkl","mn","o"]  -> ["o","de","de","mn","abc","fgh","ijkl"]
lsort :: [[a]] -> [[a]]
lsort = sortBy (\x y -> (compare (length x) (length y)))

lsort2 :: [[a]] -> [[a]]
lsort2 = sortBy (comparing length)

lsortSpec :: Spec
lsortSpec = do
  describe "Sorting a list of lists according to length of sublists." $ do
     it "lsort first solution." $ do
       lsort ["abc","de","fgh","de","ijkl","mn","o"] `shouldBe` ["o","de","de","mn","abc","fgh","ijkl"]
     it "lsort second solution." $ do
       lsort2 ["abc","de","fgh","de","ijkl","mn","o"] `shouldBe` ["o","de","de","mn","abc","fgh","ijkl"]


lfsort :: [[a]] -> [[a]]
-- length frequency -> go though each and grouby their length and then sort each with lsort and concat
lfsort = concat groups
  where groups =

lfsortSpec :: Spec
lfsortSpec = do
   describe "sort the elements of this list according to their length frequency"
     it "first solution for length frequency sort" $ do
         lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] `shouldBe` ["ijkl","o","abc","fgh","de","de","mn"]


-- suppose that a list contains elements that are lists themselves.
-- sort the elements of this list according to their length frequency;
-- i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
hask
