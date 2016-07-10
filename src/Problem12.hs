{-# LANGUAGE TemplateHaskell #-}

module NinetyNine.Problem12 where

import Data.List

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.All

import NinetyNine.Problem11

-- Decode a run-length encoded list.

decodeS xs = map decodeFn xs
   where
     decodeFn (Single x) = (1,x)
     decodeFn (Multiple n x) = (n,x)

decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified xs = concat (map (\(n,x) -> take n (repeat x)) (decodeS xs))

decodeModified2 :: [ListItem a] -> [a]
decodeModified2 = concatMap decodeHelper
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x

toTuple :: ListItem a -> (Int, a)
toTuple (Single x) = (1,x)
toTuple (Multiple n x) = (n,x)

decodeModified3 :: [ListItem a] -> [a]
decodeModified3 = concatMap (uncurry replicate . toTuple)

decodeModified4 :: [ListItem a] -> [a]
decodeModified4 = foldl (\x y -> x ++ decodeHelper' y) []
  where
    decodeHelper' (Single x) = [x]
    decodeHelper' (Multiple n x) = replicate n x

decodeModified5 :: [ListItem a] -> [a]
decodeModified5 = foldl (\acc e -> case e of Single x -> acc ++ [x]; Multiple n x -> acc ++ replicate n x) []

-- Test
decodeModifiedSpec :: Spec
decodeModifiedSpec = do
  describe "decodeModified" $ do
    it "Decode a run-length encoded list." $ do
        decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
            `shouldBe` "aaaabccaadeeee"

    it "[decodeModified2] Decode a run-length encoded list." $ do
        decodeModified2 [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
            `shouldBe` "aaaabccaadeeee"

    it "[decodeModified3] Decode a run-length encoded list." $ do
        decodeModified3 [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
            `shouldBe` "aaaabccaadeeee" 

    it "[decodeModified4] Decode a run-length encoded list." $ do
        decodeModified4 [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
            `shouldBe` "aaaabccaadeeee" 

    it "[decodeModified5] Decode a run-length encoded list." $ do
        decodeModified5 [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
            `shouldBe` "aaaabccaadeeee" 

-- quickCheck
prop_decodeRoundtrip xs = decodeModified (encodeModified xs) == xs

return []
main = $quickCheckAll

