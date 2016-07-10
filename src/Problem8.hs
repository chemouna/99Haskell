module NinetyNine.Problem8 where

import Control.Monad
import Data.List

import Test.Hspec

-- Eliminate consecutive duplicates of list elements.
-- exple : compress "aaaabccaadeeee" => "abcade" 

compress :: Eq a => [a] -> [a]
compress = map head . group

-- solution 2
compress2 (x:ys@(y:_))
    | x == y    = compress2 ys
    | otherwise = x : compress2 ys
compress2 ys = ys

-- solution 3
compress3 :: Eq a => [a] -> [a]
compress3 [] = []
compress3 (x:xs) = x : (compress3 $ dropWhile (== x) xs)

-- solution 4
compress4 :: Eq a => [a] -> [a]
compress4 x = foldr (\a b -> if a == (head b) then b else a:b) [last x] x

-- solution 5
compress5 :: Eq a => [a] -> [a]
compress5 x = foldl (\a b -> if (last a) == b then a else a ++ [b]) [head x] x

-- solution 6
compress6 :: Eq a => [a] -> [a]
compress6 x = reverse $ foldl (\a b -> if (head a) == b then a else b:a) [head x] x

--

compressSpec :: Spec
compressSpec = do
  describe "compress" $ do
    it "Eliminate consecutive duplicates of list elements." $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"

    it "[With compress2] Eliminate consecutive duplicates of list elements." $ do
      compress2 "aaaabccaadeeee" `shouldBe` "abcade"

    it "[With compress3] Eliminate consecutive duplicates of list elements." $ do
      compress3 "aaaabccaadeeee" `shouldBe` "abcade"

