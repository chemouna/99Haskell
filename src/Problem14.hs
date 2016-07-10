{-# LANGUAGE TemplateHaskell #-}

module NintetyNine.Problem14 where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All

import Control.Applicative
import Data.Foldable
import Data.List

-- Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

dupli2 :: [a] -> [a]
dupli2 = concatMap (\x -> [x,x])

dupli3 :: [a] -> [a]
dupli3 xs = xs >>= (\x -> [x,x])

dupli4 :: [a] -> [a]
dupli4 xs = concat [[x,x] | x <- xs]

dupli5 = (<**> [id,id])

dupli6 :: [a] -> [a]
dupli6 = foldl (\acc x -> acc ++ [x,x]) []

dupli7 :: [a] -> [a]
dupli7 = foldr (\ x xs -> x:x:xs) []

dupli8 :: [a] -> [a]
dupli8 = foldr (\x -> (x:) . (x:)) []

dupli9 :: [a] -> [a]
dupli9 = foldr ((.) <$> (:) <*> (:)) []


dupliSpec :: Spec
dupliSpec = do
  describe "Duplicate the elements of a list." $ do
    it "Decode a run-length encoded list." $ do
        dupli [1,2,3] `shouldBe` [1,1,2,2,3,3]
    it "[With dupli2] Decode a run-length encoded list." $ do
        dupli2 [1,2,3] `shouldBe` [1,1,2,2,3,3]
    it "[With dupli3] Decode a run-length encoded list." $ do
        dupli3 [1,2,3] `shouldBe` [1,1,2,2,3,3]
    it "[With dupli4] Decode a run-length encoded list." $ do
        dupli4 [1,2,3] `shouldBe` [1,1,2,2,3,3]
    it "[With dupli5] Decode a run-length encoded list." $ do
        dupli5 [1,2,3] `shouldBe` [1,1,2,2,3,3]
    it "[With dupli6] Decode a run-length encoded list." $ do
        dupli6 [1,2,3] `shouldBe` [1,1,2,2,3,3]
    it "[With dupli7] Decode a run-length encoded list." $ do
        dupli7 [1,2,3] `shouldBe` [1,1,2,2,3,3]
    it "[With dupli8] Decode a run-length encoded list." $ do
        dupli7 [1,2,3] `shouldBe` [1,1,2,2,3,3]
    it "[With dupli9] Decode a run-length encoded list." $ do
        dupli7 [1,2,3] `shouldBe` [1,1,2,2,3,3]


-- QuickCheck
-- size after applying dupli should double
prop_dupliShouldDoubleSize xs = length (dupli9 xs) == length xs * 2

-- every two even index element should be equal to the odd one after it
-- prop_evenIndexEqualsOddAfterIt (NonEmpty xs) = map (dupli xs) 

-- in haskell how can i divide a list into buckets of 2 ? 

-- if you remove all odd index elements you should have the same list
-- prop_dupliListRemoveOddIndexSame xs = filter (dupli xs) -- problem : i dont know how to filter by index in a list ? 

return []
main = $quickCheckAll


